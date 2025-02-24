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

## 1) MODEL ---------------------------------------------------------------- ----

bounded_confidence <- function(n_agents, n_steps, confidence_threshold, hardening, pieces_of_info, initial_opinions) {
  opinions <- matrix(NA, nrow = n_agents, ncol = n_steps + 1)
  opinions[, 1] <- initial_opinions
  
  for(i in 1:n_agents){
    # create a data.frame with pieces_of_info number of informations and a boolean whether they have been seen or not
    info <- data.frame(info = runif(pieces_of_info, -1, 1), seen = FALSE)
    
    for(step in 1:n_steps){
      # compute individual threshold at given time
      threshold_of_i <- confidence_threshold * exp(-hardening * opinions[i,step]^2)
      
      # create set of unseen pieces of information
      unseen_info <- info$info[!info$seen]
      # select an unseen piece of info if any
      if(length(unseen_info) > 0){
        j <- sample(unseen_info, 1)
        # set seen to TRUE
        #info$seen[info$info == j] <- TRUE
      } else {
        # if no unseen info, set j to current opinion
        j <- opinions[i,step]
      }
      
      # update opinion if within confidence threshold
      if(abs(opinions[i,step] - j) <= threshold_of_i){
        opinions[i,step+1] <- opinions[i,step] + 0.5 * (j - opinions[i,step])
      } else {
        opinions[i,step+1] <- opinions[i,step]
      }
    }
  }
  
  return(opinions)
}


## 2) FUNCTIONS ------------------------------------------------------------ ----

### Simulation and plot function ----

sim_and_plot <- function(n_agents = 500,
                         n_steps = 100,
                         confidence_threshold = 0.3,
                         hardening = 0,
                         pieces_of_info = 50,
                         initial_opinions = rep(0, 100),
                         title = "",
                         subtitle = NULL,
                         seed = 1234){
  set.seed(seed)
  # Simulate bounded confidence model
  opinions <- bounded_confidence(n_agents, n_steps, confidence_threshold, hardening, pieces_of_info, initial_opinions)
  opinions <- opinions[, -1]
  
  # Create data frame for ggplot
  df <- data.frame(
    agent = rep(1:n_agents, times = n_steps),
    step = rep(1:n_steps, each = n_agents),
    opinion = as.vector(opinions)
  )
  
  # Subset to every Nth timestep
  df_subset <- df[df$step %% floor(n_steps / 5) == 0, ]
  
  # set subtitle
  if(is.null(subtitle)){
    subtitle <- TeX(paste0("items = ", pieces_of_info, ", $\\beta$ = ", hardening, ", $\\epsilon_M$ = ", confidence_threshold))
  }
  
  # Plot using ggridges
  p <- ggplot(df_subset, aes(x = opinion, y = as.factor(step))) +
    geom_density_ridges_gradient(aes(fill= ..x..), scale = 1.5, rel_min_height = 0.02, bandwidth = 0.02, color='white', lwd=0.5) +
    scale_fill_gradientn(colours =  c("red","red","orange","green","orange","red","red"), limits=c(-1,1)) +
    scale_x_continuous(limits = c(-1,1), breaks=seq(-1, 1, 1), labels=c("-1","0","+1")) +
    labs(title = "",
         x = "Attitudes",
         y = "Item consultations") +
    ggtitle(title,subtitle)
  return(p)
} 


### Simulation experiment function ----

sim_exp <- function(
    n_agents = 500,
    n_steps = 500,
    confidence_threshold = 0.4,
    hardening = 0,
    pieces_of_info = 20,
    initial_opinion = 0) {
  # df to store results
  df <- expand.grid(
    n_agents = n_agents,
    n_steps = n_steps,
    confidence_threshold = confidence_threshold,
    hardening = hardening,
    pieces_of_info = pieces_of_info,
    initial_opinion = initial_opinion
  )
  
  # measurement vars
  df$mean_abs_opinion <- NA
  df$sd_abs_opinion <- NA
  
  # simulate
  for(i in c(1:nrow(df))){
    n_agents = df$n_agents[i]
    bcm <- bounded_confidence(
      n_agents = n_agents, 
      n_steps = df$n_steps[i], 
      confidence_threshold = df$confidence_threshold[i], 
      hardening = df$hardening[i], 
      pieces_of_info = df$pieces_of_info[i], 
      initial_opinions = rep(df$initial_opinion[i], n_agents)
    )
    df$mean_abs_opinion[i] <- mean(abs(bcm))
    df$sd_abs_opinion[i] <- sd(abs(bcm))
    cat("\r", paste0(floor(i / nrow(df) * 100), '% completed, (', i, ' / ', nrow(df), ' conditions)'))
    flush.console() 
  }
  cat('finished\n')  
  return(df)
}


### Trajectory plot function ----

traj_plot <- function(
    n_steps=150, 
    confidence_threshold=0.4, 
    hardening=3, 
    pieces_of_info=100, 
    title=NULL, 
    subtitle=NULL, 
    seed=NULL){
  if(is.null(seed)){
    seed <- sample(1:1000, 1)
    title <- seed
  } 
  set.seed(seed)
  df <- data.frame(opinion=rep(NA,n_steps+1), threshold=rep(NA,n_steps+1), j=rep(NA,n_steps+1), accepted=rep(NA,n_steps+1))
  df$opinion[1] <- 0
  
  # create a data.frame with pieces_of_info amount of information
  info <- runif(pieces_of_info, -1, 1)
  
  for(step in 1:n_steps){
    # compute individual threshold at given time
    df$threshold[step] <- confidence_threshold * exp(-hardening * df$opinion[step]^2)
    df$j[step] <- sample(info, 1)
    if(abs(df$opinion[step] - df$j[step]) <= df$threshold[step]){
      df$opinion[step + 1] <- (1 - df$threshold[step]) * df$opinion[step] + df$threshold[step] * df$j[step]
      df$accepted[step] <- "item accepted"
    } else {
      df$opinion[step + 1] <- df$opinion[step]
      df$accepted[step] <- "item ignored"
    }
  }
  df$accepted[is.na(df$accepted)] <- "item ignored"
  df$threshold[n_steps+1] <- confidence_threshold * exp(-hardening * df$opinion[n_steps+1]^2)
  
  if(is.null(subtitle)){
    subtitle <- TeX(paste0("items = ", pieces_of_info, ", $\\beta$ = ", hardening, ", $\\epsilon_M$ = ", confidence_threshold))
  }
  
  p <- ggplot(df, aes(x=1:nrow(df), y=opinion)) +
    geom_ribbon(aes(ymin=opinion-threshold,ymax=opinion+threshold), fill="#f5f5f5") +
    geom_line(aes(y=opinion-threshold), color='grey80', linewidth=0.25) +
    geom_line(aes(y=opinion+threshold), color='grey80', linewidth=0.25) +
    geom_point(aes(y=j, color=j, shape=accepted), size=1) +
    geom_line(color='white', lwd=2, alpha=.8) +
    geom_line(color='black', lwd=0.4) +
    geom_rug(sides='l', mapping = aes(y=j, color = j)) +
    guides(color=FALSE) +
    scale_color_gradientn(colours =  c("red","red","orange","green","orange","red","red"), limits=c(-1,1)) +
    scale_shape_manual(values=c(19,4)) +
    labs(title = title, subtitle = subtitle, x = "Item consultations", y = "Attitude", shape="") +
    scale_y_continuous(limits=c(-1,1), breaks=c(-1,0,1)) +
    coord_flip(clip='off') +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.position='top',
      legend.background=element_rect(
        fill="#f5f5f5", size=0, color="#f5f5f5"),
      legend.text=element_text(color="black"),
      legend.margin=margin(-1,5,0,-8))
  
  return(p)
}

traj_plot()
## 3) SIMULATIONS ---------------------------------------------------------- ----

### Simulation and (ridge)plot 2 scenario's ----

(p3 <- sim_and_plot(pieces_of_info=10,  confidence_threshold=0.4, hardening=3, n_steps = 150, n_agents=10000, title="", subtitle="Evolution of attitudes in 10,000 independent runs"))
(p4 <- sim_and_plot(pieces_of_info=100, confidence_threshold=0.4, hardening=3, n_steps = 150, n_agents=10000, title="", subtitle="Evolution of attitudes in 10,000 independent runs"))

### Trajectory plots ----

(p1 <- traj_plot(n_steps=150, confidence_threshold=0.4, hardening=3, pieces_of_info=10,  title="Limited information (N=10)", subtitle="Example trajectory", seed=223))
(p2 <- traj_plot(n_steps=150, confidence_threshold=0.4, hardening=3, pieces_of_info=100, title="Abundant information (N=100)", subtitle="Example trajectory", seed=673))

### Combined plots ----

(p1 + p2) / (p3 + p4) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 12, face='bold'),
        plot.tag.position  = c(.025, .94))

ggsave("BCmodel.pdf", width = 7, height = 8.4, units = "in")

