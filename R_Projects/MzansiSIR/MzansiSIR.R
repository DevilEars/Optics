#devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)
data(coronavirus)


# N = number of individuals in population

# This works, within their ranges it fits well
# myCountry = "Belgium"
# N = 11515793
#sir_start_date = "2020-02-04"
#sir_end_date = "2020-03-30"

# I want to model South Africa (2019 population estimate from Wokipedia)
myCountry = "South Africa"
N = 58775022

# Date range, this is as much data I currently have in coronavirus package
sir_start_date = "2020-02-04"
sir_end_date = "2020-07-31"

# This was 0.5 in the original
interval = 0.05

# S = Susceptible
# I = Infected
# R = Recovered, or RIP

# SIR = function(time, state, parameters){
#     par = as.list(c(state, parameters))
#     with(par, {
#         # Susceptible individuals decrease as infection spreads
#         # at rate beta
#         dS = -beta * I * S / N
#         # Infected individuals increase at rate beta that the 
#         # infected have contact with Susceptible
#         # minus the previously Infected people who recover at rate gamma
#         dI = beta * I * S / N - gamma * I
#         # recovered individuals increases at the recovery rate of 
#         # infected individuals
#         dR = gamma * I
#         list(c(dS, dI, dR))
#     })
# }

SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
        dS <- -beta * I * S / N
        dI <- beta * I * S / N - gamma * I
        dR <- gamma * I
        list(c(dS, dI, dR))
    })
}

#doing black magic, see
# https://www.rdocumentation.org/packages/magrittr/versions/1.5
`%>%` <- magrittr::`%>%`


# extract the cumulative incidence
df <- coronavirus %>%
    dplyr::filter(country == myCountry) %>%
    dplyr::group_by(date, type) %>%
    dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
    tidyr::pivot_wider(
        names_from = type,
        values_from = total
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(active = confirmed - death - recovered) %>%
    dplyr::mutate(
        confirmed_cum = cumsum(confirmed),
        death_cum = cumsum(death),
        recovered_cum = cumsum(recovered),
        active_cum = cumsum(active)
    )

# put the daily cumulative incidence numbers for myCountry from
# start date to end date in a library called Infected
library(lubridate)

Infected = subset(df, date >= ymd(sir_start_date) & date <= ymd(sir_end_date))$active_cum

# Create Day vector the same length as our
# cases vector
Day = 1:(length(Infected))

# initial values for S, I and R
init = c(
    S = N - Infected[1],
    I = Infected[1],
    R = 0
)

# define a function to calculate the residual sum of squares
# (RSS), passing in parameters beta and gamma that are to be
# optimised for the best fit to the incidence data

# ode is the ordinary differential equation solver
# RSS = function(parameters) {
#     names(parameters) = c("beta", "gamma")
#     out = ode(y = init, times = Day, func = SIR, parms = parameters)
#     fit = out[, 3]# I love magic numbers, don't you?
#     sum((Infected - fit)^2)#this is a sum of squares so OK
# }

RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[, 3]
    sum((Infected - fit)^2)
}

# find the values of beta and gamma that give the
# smallest RSS, which represents the best fit to the data.

# Start with values of 0.5 for each, and constrain them to
# the interval 0 to 1.0

#install.packages("deSolve")
library(deSolve)

Opt <- optim(c(interval, interval),
             RSS,
             method = "L-BFGS-B",
             lower = c(0, 0),
             upper = c(1, 1)
)

# check for convergence
# "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL" means yes
Opt$message

# Examine the fitted values for Beta and Gamma
# Don't include weird characters, stick to ASCII

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par


# Beta controls the transition between S and I (i.e., susceptible and infectious) and 
# Gamma controls the transition between I and R (i.e., infectious and recovered). 
# 
# However, those values do not mean a lot but we use them to get the fitted 
# numbers of people in each compartment of our SIR model for the dates that 
# were used to fit the model, and compare those fitted values with the 
# observed (real) data

# time in days for predictions
t = 1:as.integer(ymd(sir_end_date) + 1 - ymd(sir_start_date))

# get the fitted values from our SIR model
fitted_cumulative_incidence = data.frame(ode(
    y = init, 
    times = t,
    func = SIR, 
    parms = Opt_par
))

# add a Date column and the observed incidence data
library(dplyr)

fitted_cumulative_incidence <- fitted_cumulative_incidence %>%
    mutate(
        Date = ymd(sir_start_date) + days(t - 1),
        Country = myCountry,
        cumulative_incident_cases = Infected
    )

# plot the data
library(ggplot2)

fitted_cumulative_incidence %>%
    ggplot(aes(x = Date)) +
    geom_line(aes(y = I), colour = "red") +
    geom_point(aes(y = cumulative_incident_cases), colour = "blue") +
    labs(
        y = "Cumulative incidence",
        title = paste("COVID-19 fitted vs observed cumulative incidence,", myCountry),
        subtitle = "(Red = fitted from SIR model, blue = observed)"
    ) +
    theme_minimal()

