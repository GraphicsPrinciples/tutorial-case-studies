library(tidyverse)
library(RxODE)
library(caTools)
library(rstanarm)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(data.table)
library(grid)
library(RColorBrewer)
#library(plotrix)  --- needed?


theme_set(theme_linedraw(base_size=12))
lbr <- scales::trans_breaks("log10", function(x) 10^x)
llb <- scales::trans_format("log10", scales::math_format(10^.x))

n <- 30
r <- 2/3
id <- 1:n
ztext <- factor(c("Placebo", "Active"), levels=c("Placebo","Active"))
dx <- tibble(id = id, 
             x1 = rnorm(n,0,.1),
             x2 = rnorm(n,1,.2),
             z  = c(rep(0,n*(1-r)), rep(1,n*r)),
             a  = rnorm(n, 10, 2),
             bm = -z*0.75,
             b  = rnorm(n, bm, 1))
time <- 0:4
dres <- as_tibble(expand.grid(id=1:n, time=time)) %>% 
  inner_join(dx, by="id") %>%
  mutate(#ymean = a + time*b + x1 + x2,
    baseline = a + x1 + x2,
    incrmean=b) %>% 
  #       cfb = y - a) %>%
  arrange(id, z, time)

dres$incr <- with(dres, rnorm(n*5, incrmean, 1))
dres$incr[dres$time==0] <- 0
dres <- dres %>% 
  group_by(id) %>% 
  do({
    #print(.)
    ci <- cumsum(.$incr)#; print(ci)
    csi <- ci/sqrt(1:length(time))#; print(csi)
    y <- .$baseline + csi#; print(y)
    rv <- bind_cols(., data_frame(ci, csi, y))#; print(rv)
    rv
  }) %>% 
  ungroup() %>% 
  mutate(cfb = y - baseline)

dres$ztext <- ztext[dres$z+1]

t4dat <- filter(dres, time==4) %>% 
  arrange(cfb) %>% mutate(sort_id=1:n())

ggplot(t4dat, aes(x=sort_id, y=cfb, fill=factor(ztext))) + geom_col() +
  scale_fill_brewer(palette="Dark2", name="Treatment") +
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) + 
  #geom_hline(yintercept=0, linetype=2) +
  labs(x="Subject", y="Change from baseline", 
       title="Waterfall plot of week 4 outcome by treatment")

ggsave(file=paste(fig_path, "403_a.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)


ggplot(data= t4dat, aes(x=ztext, y=cfb, colour=ztext)) + geom_boxplot(width=.55) + 
  geom_jitter(alpha=0.35, width=0.1) + scale_colour_brewer(palette="Dark2") + 
  theme(legend.position="none") + labs(x="", y="Change from baseline") + 
  theme(axis.ticks.x=element_blank(), panel.grid.major.x=element_blank())

ggsave(file=paste(fig_path, "403_b.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)



md <- dres %>% group_by(time, ztext) %>% summarise(m=mean(y), s=sd(y), n=n(), se=s/sqrt(n))
ggplot() + 
  geom_line(data=dres, aes(x=time, y=y, group=id, colour=factor(ztext)), alpha=0.35) + #geom_point(alpha=0.25) +
  #geom_smooth(data=dres, method="loess", aes(x=time, y=y, colour=factor(ztext)), se=FALSE) + 
  geom_point(data=md, aes(x=time, y=m, colour=factor(ztext)), size=2.5) + 
  #geom_point(data=true_means, aes(x=time, y=tm, colour=factor(ztext)), size=2.5, shape=2) + 
  scale_colour_brewer(palette="Dark2", name="Treatment") +
  labs(x="Week", y="Outcome", title="")

ggsave(file=paste(fig_path, "403_c.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)


ggplot(md, aes(x=time, y=m, ymin=m-s, ymax=m+s, colour=ztext)) + 
  geom_pointrange(position=position_dodge(0.3)) + 
  geom_line(stat="smooth", method="lm", position=position_dodge(0.3), se=FALSE, alpha=0.35) + 
  scale_colour_brewer(palette="Dark2", name="Treatment") + 
  labs(x="Week", y="Mean outcome (SD)", title="")

ggsave(file=paste(fig_path, "403_d.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)


mod <- readRDS("data/mod.rds")
adat <- dres %>% filter(time > 0)

if (FALSE) {
  mod <- stan_lmer(y ~ time*factor(z) + baseline + x1 + x2 + (time | id), 
                   data=adat, chains=4, iter=500, cores=4)
  print(mod)
  saveRDS(mod, "data/mod.rds")
}

nd1 <- dres %>% select(id, time, baseline, x1, x2, z) 
nd2 <- dres %>% select(id, time, baseline, x1, x2, z) %>%
  mutate(z=1-z)
nd <- bind_rows(nd1, nd2) %>%
  arrange(id, time, z)
prs <- c(0.05,0.5,0.95)
ppnd <- nd %>% select(id, time, z) %>%
  #bind_cols(as_tibble(t(posterior_predict(mod, newdata=nd, re.form=~0)))) %>%
  bind_cols(as_tibble(t(posterior_linpred(mod, newdata=nd, re.form=~0)))) %>%
  gather(4:1003, key="ppid", value="ypred") %>%
  spread(z, ypred) %>% 
  mutate(contr=`1`-`0`) %>% 
  group_by(ppid, time) %>%
  do({
    m0 <- mean(.$`0`)
    m1 <- mean(.$`1`)
    mc <- mean(.$contr)
    as_tibble(cbind(m0, m1, mc))
  }) %>% ungroup() %>%
  group_by(time) %>%
  do({
    q0 <- t(quantile(.$m0, probs=prs))
    q1 <- t(quantile(.$m1, probs=prs))
    contr <- t(quantile(.$mc, probs=prs))
    as_tibble(rbind(q0, q1, contr)) %>% mutate(var=c("Placebo", "Active", "Contrast"))
  })


ppnd %>% filter(var != "Contrast" ) %>% 
  ggplot(aes(x=time, y=`50%`, ymin=`5%`, ymax=`95%`, colour=var)) + 
  geom_pointrange(position=position_nudge(x=c(-0.1,0.1))) + 
  scale_color_brewer(palette="Dark2", name="") +
  theme(legend.position=c(0.15,0.225)) +
  labs(x="Week", y="Outcome", 
       title="Active group improves over time; placebo constant")#, 
#caption="Plot shows posterior medians with 90% CIs") 

ggsave(file=paste(fig_path, "403_e.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)



ppnd %>% filter(var == "Contrast" & time > 0) %>% 
  ggplot(aes(x=time, y=`50%`)) + 
  geom_ribbon(aes(ymin=`5%`, ymax=`95%`), fill="blue", alpha=0.15) + 
  geom_point(size=1.5) + geom_line(size=1)+ 
  geom_hline(yintercept=0, linetype=2)  + 
  labs(x="Week", y="Treatment difference", title="Treatment effect increases over time") +
  #geom_segment(aes(xend=4, yend=0), arrow=arrow(length = unit(2,"cm")))
  geom_segment(aes(x=1.00, y=-0.3, xend=1.00, yend=-3.5), arrow=arrow(), alpha=0.25) + 
  #geom_text(aes(x=0.4, y=-5.8), label="Favors treatment", )
  annotate("text", label="Greater benefit", x=1.45, y=-3.4, size=5, alpha=0.75)

ggsave(file=paste(fig_path, "403_f.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)


tdat <- nd %>% select(id, time, z) %>%
  bind_cols(as_tibble(t(posterior_predict(mod, newdata=nd, re.form=~0)))) %>%
  gather(4:1003, key="ppid", value="ypred") %>%
  spread(z, ypred) %>% 
  mutate(contr=`1`-`0`)

ggplot(dres, aes(x=time, y=y, colour=factor(z))) + facet_wrap(~id) + geom_point()

ggsave(file=paste(fig_path, "403_g.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)


ggplot(dres, aes(x=time, y=cfb, group=id, colour=factor(z))) + geom_point() + 
  geom_line() 

ggsave(file=paste(fig_path, "403_h.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)


ggplot(dres, aes(x=factor(z) , y=cfb)) + geom_boxplot(width=0.5) + geom_jitter(width=0.1, alpha=0.5)

dmod <- data_frame(id=dx$id, 
                   b = dx$b,
                   z = dx$z,
                   best = summary(mod, regex_pars="b\\[time")[,"50%"]) %>%
  arrange(best) %>% mutate(sort_id=1:n())
ggplot(dmod, aes(x=sort_id, y=best, fill=factor(z))) + geom_col() +
  scale_fill_brewer(palette="Dark2", name="Treatment") +
  labs(x="Patient ID", y="Change from baseline")

ggplot(dmod, aes(x=factor(z), y=best)) + geom_boxplot(width=0.5) + geom_jitter(width=0.1, alpha=0.5)

ggplot(dx, aes(x=factor(z), y=b)) + geom_boxplot(width=0.5) + geom_jitter(width=0.1, alpha=0.5)

dx$best <- summary(mod, regex_pars="b\\[time")[,"50%"]
ggplot(dx, aes(x=b, y=best)) + geom_point() + geom_smooth(method="lm") + 
  geom_abline(yintercept=0, slope=1)

