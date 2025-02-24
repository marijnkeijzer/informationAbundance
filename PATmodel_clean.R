## Code for `How opinions get more extreme in an age of information abundance' #
## Deffuant, Keijzer & Banisch                                     by MK, 2025 #

library("ggplot2")
library("ggridges")
library("truncnorm")
library("patchwork")
library("latex2exp")

theme_set(
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_text(face='bold', size=7),
        axis.text = element_text(size=7),
        plot.title = element_text(face='bold', size=11),
        plot.subtitle = element_text(size=8),
        legend.text = element_text(size=8))
)

## 1) MODEL --------------------------------------------------------------- ----

pat_sim <- function(n_steps=100, n_items=10, alpha=0.3, beta=0.3, seed=NULL){
  if(is.null(seed)){
    seed <- sample(100:999, 1)
  } else if (seed == "print"){
    seed <- sample(100:999, 1)
    print(paste("seed =", seed))
  }
  set.seed(seed)
  
  neg_items <- 1:(n_items / 2)
  pos_items <- (n_items / 2 + 1):n_items
  attitude <- rep(0, n_steps)
  information <- matrix(0, nrow=n_steps, ncol=n_items)
  item_sequence <- rep(NA, n_steps)
  step <- 1
  
  while(step < n_steps){
    step <- step + 1
    information[step,] <- information[step - 1,] 
    current_item <- sample(1:n_items, 1)
    item_sequence[step] <- current_item
    balance_of_beliefs <- (sum(information[step, pos_items]) - sum(information[step, neg_items])) * alpha
    vv <- exp(- beta * balance_of_beliefs)
    attitude[step] <- (1-vv) / (1+vv)
    current_item_sign <- ifelse(current_item <= n_items / 2, -1, 1)
    p_belief <- 1 / (1 + exp(- beta * balance_of_beliefs * current_item_sign))
    if(runif(1) < p_belief) {
      information[step, current_item] <- 1
    } else {
      information[step, current_item] <- 0
    }
  }
  return(list("attitude"=attitude, "information"=information, "item_sequence"=item_sequence))
}

## 2) FUNCTIONS ----------------------------------------------------------- ----

#### Plotting distributions over time ----

sim_and_plot <- function(n_agents = 500,
                         n_steps = 100,
                         alpha = 0.3,
                         beta = 0.5,
                         pieces_of_info = 500,
                         title = "",
                         subtitle = NULL,
                         plottype = "opinions",
                         seed = 1234) {
  # Run the simulation
  opinions <- matrix(NA, nrow=n_agents, ncol=n_steps)
  
  for(i in c(1:n_agents)) {
    opinions[i,] <- pat_sim(n_steps=n_steps, n_items=pieces_of_info, alpha=alpha, beta=beta)$attitude
  }
  
  # Create data frame for ggplot
  df <- data.frame(
    agent = rep(1:n_agents, times = n_steps),
    step = rep(1:n_steps, each = n_agents),
    opinion = as.vector(opinions)
  )
  
  # Subset to every Nth timestep
  df_subset <- df[df$step %% floor(n_steps / 10) == 0, ]
  
  # set bin size
  seri <- c()
  serimax <- 11
  for(i in 2:serimax){
    seri <- c(seri,log(i) / log(serimax))
  }
  seri <- c(-rev(seri), seri)
  
  # set subtitle
  if(is.null(subtitle)){
    subtitle <- TeX(paste0("$\\beta$ = ", beta, ", items = ", pieces_of_info))
  }
  
  # Plot using ggridges
  p <- ggplot(df_subset, aes(x = opinion, y = as.factor(step))) +
    #geom_density_ridges_gradient(stat = "binline", breaks=seri, aes(fill= ..x..), scale = 2, draw_baseline = FALSE, color='white', linewidth=0.5) +
    geom_density_ridges_gradient(stat = "binline", bins=31, aes(fill= ..x..), scale = 2, draw_baseline = FALSE, color='white', linewidth=0.5) +
    geom_density_ridges_gradient(fill=NA, scale = 1.9, rel_min_height = 0.008, bandwidth = 0.08, color='black', lwd=0.25) +
    scale_fill_gradientn(colours = c("red","orange","green","orange","red"), limits=c(-1.1,1)) +
    scale_x_continuous(limits = c(-1.1,1.1), breaks=seq(-1, 1, 1), labels=c("-1","0","+1")) +
    labs(title = "",
         x = "Attitudes",
         y = "Item consultations") +
    ggtitle(title, subtitle)
  
  return(p)
}

#### Trajectory plot ----

pat_plot <- function(sim_object, alpha=0.3, beta=0.5, seed=NULL, title="", subtitle=""){
  attitudes <- sim_object$attitude
  info <- sim_object$information
  item_sequence <- sim_object$item_sequence - 1
  
  neg_items <- c(1:(ncol(info) / 2))
  pos_items <- c((ncol(info) / 2 + 1):ncol(info))
  
  # melt matrix into long data frame
  info <- as.data.frame(info)
  info$step <- 1:nrow(info)
  info <- reshape2::melt(info, id.vars='step')
  info$variable <- as.numeric(info$variable) - 1
  info$pos <- info$variable / max(info$variable) * 2 - 1
  #info$pos[info$pos <= 0] <- info$pos[info$pos <= 0] - 2 / max(info$variable)
  
  # rescaling
  smallest_val <- min(info$pos[info$pos > 0])
  biggest_val <- max(info$pos)
  info$pos[info$pos>0] <- info$pos[info$pos>0] - (biggest_val - abs(info$pos[info$pos>0])) * smallest_val
  info$pos[info$pos<0] <- info$pos[info$pos<0] + (biggest_val - abs(info$pos[info$pos<0])) * smallest_val
  info$pos <- info$pos * 0.9
  info$pos[info$pos < 0] <- info$pos[info$pos < 0] - 0.1
  info$pos[info$pos > 0] <- info$pos[info$pos > 0] + 0.1
  
  info$focal <- 0
  info$size <- 0.05
  info$color <- "#e2e2e2"
  info$color[info$value==1] <- "#00EE00"
  for(i in 1:length(item_sequence)){
    info$focal[info$step == i & info$variable == item_sequence[i]] <- 1
    info$size[info$step == i & info$variable == item_sequence[i]] <- 1
    info$color[info$step == i & info$variable == item_sequence[i]] <- "#a7a7a7"
    info$color[info$step == i & info$variable == item_sequence[i] & info$value == 1] <- "#00BF00"
  }
  #info$color[info$value==1] <- "green"

  p <- ggplot() +
    geom_polygon(data=data.frame(x=c(-1.5, -1.5, length(item_sequence)+10,length(item_sequence)+10), y=c(-1.05, -0.05, -0.05, -1.05)), aes(x=x, y=y), fill='#f5f5f5') + #, color='grey80', lwd=0.25) +
    geom_polygon(data=data.frame(x=c(-1.5, -1.5, length(item_sequence)+10,length(item_sequence)+10), y=c( 1.05,  0.05,  0.05,  1.05)), aes(x=x, y=y), fill='#f5f5f5') + #, color='grey80', lwd=0.25) +
    #geom_point(data=info, aes(y=pos, x=step, shape=as.character(value), color=as.character(value)), size=info$size) +
    geom_point(data=info[info$focal==0,], aes(y=pos, x=step, shape=as.character(value)), color=info[info$focal==0,]$color, size=info[info$focal==0,]$size) +
    geom_point(data=info[info$focal==1,], aes(y=pos, x=step, shape=as.character(value)), color=info[info$focal==1,]$color, size=info[info$focal==1,]$size) +
    geom_line(data=data.frame(x=0:length(item_sequence), y=c(0,attitudes)), aes(x=x, y=y), color='white', lwd=2, alpha=.5) +
    geom_line(data=data.frame(x=0:length(item_sequence), y=c(0,attitudes)), aes(x=x, y=y), color='black', lwd=0.4) +
    scale_y_continuous(limits=c(-1.1,1.1), breaks=c(-1,0,1), labels=c("-1","0","+1")) +
    #scale_color_manual(values=c("#efefef","green")) +
    scale_shape_manual(values=c(4, 19)) +
    coord_flip() +
    labs(title = title, subtitle = subtitle, x = "Item consultations", y = "Attitude", shape="") +
    annotate(geom="text", x=max(info$step)+5.5, y= 0.55, label="positive items", color="black", size=2.8) +
    annotate(geom="text", x=max(info$step)+5.5, y=-0.55, label="negative items", color="black", size=2.8)
  
  return(p)
}
pat_plot(pat_sim(n_steps=150, n_items=100,  alpha=0.3, beta=0.5, seed=123), title="Limited information (N=10)",   subtitle="Example trajectory")

## 3) SIMULATIONS --------------------------------------------------------- ----

### Trajectory plots ----

n_steps <- 150

(p1 <- pat_plot(pat_sim(n_steps=n_steps, n_items=10,  alpha=0.3, beta=0.5, seed=123), title="Limited information (N=10)",   subtitle="Example trajectory"))
(p2 <- pat_plot(pat_sim(n_steps=n_steps, n_items=100, alpha=0.3, beta=0.5, seed=785), title="Abundant information (N=100)", subtitle="Example trajectory"))

## 4) MARKOV CHAIN VERSION ------------------------------------------------ ----

runPATDens = function(steps, N, beta = 0.5){
  na = N/2
  sm = matrix(data = rep(0, (na+1)^2), nrow = na+1, ncol = na+1)
  sm[1,1] = 1
  #  print(sm[51,51])
  st = matrix(data = rep(0, steps*(N+1)), nrow = N+1, ncol = steps)
  colnames(st) = 1:steps
  avals = exp(- beta*(-(na):na))
  avals = (1-avals)/(1+avals)
  rownames(st) = avals
  for (tt in (1:steps)){
    #cat("\r   ",tt)
    delta = matrix(data = rep(0, (na+1)^2), nrow = na+1, ncol = na+1)
    for (p in (0:na)){
      for (n in (0:na)){
        prb = 1 / (1 + exp(- beta * (p -n)))
        # print(paste("p: ", p, "  n: ", n))
        dd = sm[p+1, n+1]
        if (dd > 0){
          if (p < na){ 
            dp = dd * prb * (1 - p/na)/2
            delta[p+2, n+1] = delta[p+2, n+1] + dp
            delta[p+1, n+1] = delta[p+1, n+1] -dp
          }
          if (p > 0) { 
            dp = dd * (1 - prb) * p/ (2*na)
            delta[p, n+1] = delta[p, n+1] + dp
            delta[p+1, n+1] = delta[p+1, n+1] -dp
          }
          if (n < na){ 
            dp = dd * (1-prb) * (1 - n/na)/2
            delta[p+1, n+2] =  delta[p+1, n+2] + dp
            delta[p+1, n+1] = delta[p+1, n+1] -dp
          } 
          if (n > 0){ 
            dp = dd * prb * n/ na /2
            delta[p+1, n] = delta[p+1, n] + dp
            delta[p+1, n+1] = delta[p+1, n+1] -dp
          } 
        }
      }
    }
    sm = sm + delta
    states = rep(0, N+1)
    for (p in (0:na)){
      for (n in (0:na)){
        ii = p - n + na+1
        states[ii] = states[ii]+sm[p+1, n+1]
      }
    }
    st[,tt] = states
  } 
  return(st)
}

plot_prob_distribution <- function(num_steps=150, num_items=10){
  df <- as.data.frame(t(runPATDens(num_steps, num_items, beta = 0.15)), colnames = FALSE)
  df$step <- 1:nrow(df)
  df <- df[df$step %% floor(nrow(df) / 5) == 0, ]
  df <- reshape2::melt(df, id.vars = "step")
  df$step <- factor(df$step, levels = rev(unique(df$step)))
  df$variable <- as.numeric(as.character(df$variable))
  df$base <- 0
  
  ggplot(df, aes(x=variable, y=value, color=variable)) +
    #geom_point(shape=15, size=2) +
    geom_segment(aes(xend=variable, yend=base), size=1.5-1.4*abs(df$variable)) +
    scale_color_gradientn(colours = c("red","orange","green","orange","red"), limits=c(-1.1,1)) +
    scale_x_continuous(limits = c(-1.02,1.02), breaks=seq(-1, 1, 1), labels=c("-1","0","+1")) +
    scale_y_continuous(labels=NULL) +
    labs(title = "",
         subtitle = "Evolution of the probability distribution of attitudes",
         x = "Attitudes",
         y = "Item consultations") +
    facet_wrap(step ~ ., scales='free_y', ncol=1, strip.position="left") +
    annotate("text", x = -1.1, y = 0, label = "0", size=3) +
    theme(strip.text.y.left=element_text(size=6, vjust=0, angle=0))
}

(p3 <- plot_prob_distribution(num_steps=150, num_items=10))
(p4 <- plot_prob_distribution(num_steps=150, num_items=100))


### Combine plots ---

(p1 + p2) / (p3 + p4) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 12, face='bold'),
        plot.tag.position  = c(.025, .94))

ggsave("PATmodel.pdf", width = 7, height = 8.4, units = "in")











