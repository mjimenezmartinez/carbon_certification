library(decisionSupport)
library(ggplot2)

input_file_name <- "dualpurpose_walnut_rotation_40trees_rootnow.csv"


establishment_funded <- FALSE # choose TRUE or FALSE according to the (public) funding context of the intervention

# choose TRUE or FALSE according to the funding context of the intervention

# Modify the gompertz_yield function so that first_yield_estimate_percent is never equal or higher than second_yield_estimate_percent
# this modification is recommended whenever the function is applied to simulate the accumulation of perennial biomass on trees
ontogenic_growth_gompertz <- function (max_harvest, time_to_first_yield_estimate, time_to_second_yield_estimate, 
                                       first_yield_estimate_percent, second_yield_estimate_percent, 
                                       n_years, var_CV = 0, no_yield_before_first_estimate = TRUE) 
{
  a = max_harvest
  t1 = time_to_first_yield_estimate
  t2 = time_to_second_yield_estimate
  p2 = second_yield_estimate_percent/100
  if (p2 > 0.999) 
    p2 <- 0.999
  if (p2 < 0.001) 
    p2 <- 0.001
  if (first_yield_estimate_percent > 99) 
    first_yield_estimate_percent <- 99
  if (first_yield_estimate_percent < 1) 
    first_yield_estimate_percent <- 1
  p1 = p2*(first_yield_estimate_percent/100)
  if (t1 == t2) 
    t2 <- t1 + 1
  c <- sum(log(log(p2)/log(p1))/(t1 - t2)) # why to use sum()? The function only has one element, the output without function sum() is the same
  b <- (-log(p1)/exp(-c * t1))
  gompertz <- function(x) {
    a * exp(-b * exp(-c * x))
  }
  yield_n_years_ideal <- gompertz(1:n_years)
  yield_n_years_real <- unlist(lapply(yield_n_years_ideal, 
                                      vv, var_CV = var_CV, n = 1))
  if (no_yield_before_first_estimate & t1 > 1) {
    yield_n_years_real[1:min(c(n_years, t1 - 1))] <- 0
  }
  return(yield_n_years_real)
}

#Define the model
AF_benefit <- function(x,varnames)
{
  establishment_cost <- rep(0,n_years) #costs for setting up agroforestry systems (AF)
  maintenance_cost <- rep(0,n_years) #the annual cost spent on AF
  walnut_harvest_cost <- rep(0,n_years) #cost of harvesting walnuts and post-harvesting processes associated to farmgate marketing
  tim_harvest_cost <- rep(0,n_years) #cost of harvesting timber
  mrv_cost <- rep(0,n_years) #cost of carbon balance monitoring, reporting and certification
  growing_timber_volume <- rep(0,n_years) #the timber yield of walnut tree
  volume_change <- rep(0,n_years) #the absolute annual change of growing timber volume
  AGBcarbon_change <- rep(0,n_years) #the absolute annual change of carbon in aboveground biomass
  c_harvest <- rep(0,n_years) #the carbon contained in the harvested wood
  soil_C_stock <- rep(0,n_years) # carbon stock in the soil pool
  annual_soil_carbon_change <- rep(0,n_years) # annual change in tones per hectare
  carbon_benefit <- rep(0,n_years) #the benefit of carbon credits from the certification of carbon sequestration
  mono_annual <- rep(0,n_years) #the yield of annual crops in monocultures
  mono_annual_benefit <- rep(0,n_years) #benefits of the arable crops
  AF_annual <- rep(0,n_years) #the yield of annual crops in agroforestry
  AF_annual_benefit  <- rep(0,n_years) #the benefits of arable crops in agroforestry
  mono_wheat_benefit  <- rep(0,n_years) # the benefits of wheat in monoculture
  mono_barley_benefit  <- rep(0,n_years) # the benefits of barley in monoculture
  mono_rapeseed_benefit <- rep(0,n_years) # the benefits of rapeseed in monoculture
  AF_wheat_benefit  <- rep(0,n_years) # the benefits of wheat in agroforestry
  AF_barley_benefit  <- rep(0,n_years) # the benefits of barley in agroforestry
  AF_rapeseed_benefit <- rep(0,n_years) # the benefits of rapeseed in agroforestry
  pruning_biomass <- rep(0,n_years)
  thinning_benefit <- rep(0,n_years)
  time <- 1:n_years
  
  #Define the land area occupied by trees (as square meters in one hectare)
  n_woody_rows <- ceiling(100/(woody_row_width + crop_alley_width))
  tree_strip_area <- woody_row_width * 100 * n_woody_rows
  #as a ratio per one hectare of land
  land_share_tree_strip_area <- tree_strip_area/10000
  
  #Subsidies from the public administration
  subsidy <- rep(land_share_tree_strip_area * eco_scheme_subsidy, n_years)
  
  #Simulate the growth of the woody stand
  final_harvest <- as.numeric(1:n_years%%round(freq_tim_harvest)==0)
  #Timber growth
  growing_timber_volume <- ontogenic_growth_gompertz(max_harvest = volume_target_rotation,
                                                     time_to_first_yield_estimate = time_first_volume_est,
                                                     time_to_second_yield_estimate = time_sec_volume_est,
                                                     first_yield_estimate_percent = first_vol_rel_to_sec_est_perc,
                                                     second_yield_estimate_percent = sec_volume_est_per,
                                                     n_years = n_years,
                                                     no_yield_before_first_estimate = FALSE
  )
  
  #Simulate the production of non-wood products by the woody stand
  walnuts_yield <- gompertz_yield(max_harvest = max_walnuts_yield,
                                  time_to_first_yield_estimate = time_first_walnuts_est,
                                  time_to_second_yield_estimate = time_sec_walnuts_est,
                                  first_yield_estimate_percent = first_walnuts_est_perc,
                                  second_yield_estimate_percent = sec_walnuts_est_perc,
                                  n_years=n_years,
                                  var_CV=CV_walnuts_yield,
                                  no_yield_before_first_estimate = TRUE)
  tot_walnuts_yield <- walnuts_yield * tree_density
  
  #Calculate the costs of the intervention across the whole intervention period. In this case 60 years
  establishment_labor <- (harrowing_time + (planting_time + mulching_time)*tree_density*purchased_tree_ratio) * labor_costs
  planting_cost <- plant_material_cost*tree_density*purchased_tree_ratio
  design_cost <- runif(n = 1, min = 0, max = 4000)
  establishment_cost[1] <- establishment_cost[1] + establishment_labor + design_cost
  #establishment_cost[1] <- planting_cost + design_planning
  establishment_cost[2:n_years] <- 0
  
  maintenance_cost[1] <- 0
  maintenance_cost[2:n_years] <- weeding_and_insecticide_time * labor_costs
  maintenance_cost[c(2,3,4,5,6,8)] <- maintenance_cost[c(2,3,4,5,6,8)] + pruning_time_earlier * tree_density * labor_costs
  maintenance_cost[c(10,12)] <- maintenance_cost[c(10,12)] + pruning_time_later * tree_density * labor_costs
  maintenance_cost[first_thinning] <- maintenance_cost[first_thinning] + (thinning_time_first * growing_timber_volume[first_thinning] + forwarding_time_first_thinning) * labor_costs
  maintenance_cost[second_thinning] <- maintenance_cost[second_thinning] + (thinning_time_second * growing_timber_volume[second_thinning] + forwarding_time_second_thinning) * labor_costs
  
  walnut_harvest_cost[tot_walnuts_yield > 0] <- walnut_harv_costs + walnut_postharv_costs
  
  tim_harvest_cost[n_years] <- (tim_harv_time*volume_target_rotation + stump_removal_time*tree_density + stump_chopping_time) * labor_costs
  
  #Calculate the benefits of the intervention
  ##Wood harvest
  #pruned biomass (in tones)
  pruning_biomass[c(2,3,4,5,6,8,10,12)] <- growing_timber_volume[c(2,3,4,5,6,8,10,12)] * pruning_ratio * wood_density
  pruning_benefit <- pruning_biomass * pew_value
  
  #thinning
  thinning_benefit[first_thinning] <- growing_timber_volume[first_thinning] * thinning_intensity * wood_density * pew_value
  thinning_benefit[second_thinning] <- growing_timber_volume[second_thinning] * thinning_intensity * wood_density * pew_value
  
  #Benefit of final harvest
  #veneer_benefit <- max(growing_timber_volume) * final_harvest * bole_volume * veneer_timber_value
  pew_final <- max(growing_timber_volume) * pew_volume_final * pew_value * wood_density
  sawnwood_final <- max(growing_timber_volume) * sawnwood_volume_final * sawnwood_value
  quality_timber <- max(growing_timber_volume) * (1-sawnwood_volume_final-pew_volume_final) * quality_timber_value 
  final_harvest_benefit <- (pew_final + sawnwood_final + quality_timber) * final_harvest
  
  wood_benefit <- pruning_biomass + thinning_benefit + final_harvest_benefit
  
  tim_harvest_cost[n_years] <- (tim_harv_time*volume_target_rotation + stump_removal_time*tree_density + stump_chopping_time) * labor_costs
  
  ##Benefit of harvesting nuts
  walnuts_benefit <- tot_walnuts_yield * walnuts_value
  
  ##Benefit of the arable crops' rotation in case of no intervention, i.e.: in a field without trees
  mono_annual[c(seq(1, n_years, 3))] <- wheat_yield * rep(1,length(seq(1, n_years, 3)))
  mono_annual[c(seq(2, n_years, 3))] <- barley_yield * rep(1,length(seq(2, n_years, 3)))
  mono_annual[c(seq(3, n_years, 3))] <- rapeseed_yield * rep(1,length(seq(3, n_years, 3)))
  
  mono_annual_final <- vv(mono_annual, CV_annuals_yield, n_years)
  mono_annual_benefit[c(seq(1, n_years, 3))] <- mono_annual_final[c(seq(1, n_years, 3))] * wheat_value
  mono_annual_benefit[c(seq(2, n_years, 3))] <- mono_annual_final[c(seq(2, n_years, 3))] * barley_value
  mono_annual_benefit[c(seq(3, n_years, 3))] <- mono_annual_final[c(seq(3, n_years, 3))] * rapeseed_value
  mono_annual_benefit <- n_hectares * mono_annual_benefit
  
  ##Yield of annual crops per square meter in agroforestry systems extrapolated to one hectare, 
  AF_annual <- mono_annual_final
  #as assumed if we only consider the stabilization effect that agroforestry has on the ecosystem
  #i.e.: the impact that the coefficient of variability has on yield is eliminated in those years in which the coefficient of variability is negative
  AF_annual[which(c(mono_annual_final < mono_annual) %in% TRUE)] <- mono_annual[which(c(mono_annual_final < mono_annual) %in% TRUE)]
  # ...this effect starts only once trees are big enough to have a relevant impact on the stabilization of the ecosystem
  AF_annual[1:time_first_understory_rel_yield_reduction_est-1] <- mono_annual_final[1:time_first_understory_rel_yield_reduction_est-1]
  
  #Relative yield reduction of the understory crops that is to be expected due to tree-crop competitive outcomes if agroforestry is implemented, 
  #in relation to the hypothetical yield that would be attained if no agroforestry was implemented. 
  #Such relative reduction tends to increase with trees' age (and the assumed associated size increase), although it is also subjected to some degree of variability.
  understory_yield_reduction <- gompertz_yield(max_harvest = max_understory_rel_yield_reduction,
                                               time_to_first_yield_estimate = time_first_understory_rel_yield_reduction_est,
                                               time_to_second_yield_estimate = time_second_understory_rel_yield_reduction_est,
                                               first_yield_estimate_percent = first_understory_rel_yield_reduction_est_per,
                                               second_yield_estimate_percent = second_understory_rel_yield_reduction_per,
                                               n_years=n_years,
                                               var_CV=CV_tree_competition_on_crop,
                                               no_yield_before_first_estimate = TRUE)
  AF_annual <- AF_annual * (1-understory_yield_reduction)
  
  #Deduct the share of land which is not cultivated due to the intervention, i.e.: the share of land occupied by the woody strips
  #in order to obtain the total yield of one hectare of agroforestry in comparison to its analogue without trees
  AF_annual_final <- AF_annual * (1-land_share_tree_strip_area)
  ##Benefit of annual arable crops crops that will be obtained after the agroforestry intervention
  AF_annual_benefit[c(seq(1, n_years, 3))] <- AF_annual_final[c(seq(1, n_years, 3))] * wheat_value
  AF_annual_benefit[c(seq(2, n_years, 3))] <- AF_annual_final[c(seq(2, n_years, 3))] * barley_value
  AF_annual_benefit[c(seq(3, n_years, 3))] <- AF_annual_final[c(seq(3, n_years, 3))] * rapeseed_value
  
  # calculate annual change in Above Ground Carbon contained in Timber (assuming 0.48 carbon content in biomass)
  AGBcarbon <- growing_timber_volume  * wood_density * carbon_density
  AGBcarbon_change[1] <- AGBcarbon[1]
  AGBcarbon_change[2:n_years] <- diff(AGBcarbon)
  #calculate annual change in carbon contained in roots
  BGBcarbon <- AGBcarbon * root_to_shoot_ratio
  BGBcarbon_change <- AGBcarbon_change * root_to_shoot_ratio
  
  #calculate carbon sequestered in the soil (excluding tree roots)
  soil_C_stock[1] <- initial_soil_C_stock * max_soil_C_stock
  total_biomass_C <- AGBcarbon + BGBcarbon
  max_attainable_AGBc <- subset(input_data, input_data$variable == "volume_target_rotation")$upper * wood_density * 0.48
  max_attainable_BGBc <- max_attainable_AGBc * subset(input_data, input_data$variable == "root_to_shoot_ratio")$upper
  relative_biomass_C <- total_biomass_C/(max_attainable_AGBc + max_attainable_BGBc)
  
  for (i in 2:n_years){
    if (max_soil_C_stock > soil_C_stock[i-1]){
      soil_C_stock[i] <- soil_C_stock[i-1] + min(max_soc_seq_rate, (max_soil_C_stock - soil_C_stock[i-1]) * relative_biomass_C[i] * i/n_years)
    } else {
      soil_C_stock[i] <- soil_C_stock[i-1]
    }
  }
  annual_soil_carbon_change[2:n_years] <- diff(soil_C_stock)
  #calculate total carbon change in the simulated piece of land
  afs_c_change <- AGBcarbon_change + BGBcarbon_change + annual_soil_carbon_change
  
  #calculate annual income from selling carbon credits
  carbon_price_time_series <- vv(var_mean = co2e_price, var_CV = CV_co2_price, n = n_years, 
                                 relative_trend = co2e_price_rel_trend, lower_limit = 0, 
                                 upper_limit = highest_co2e_price)
  carbon_benefit <- afs_c_change*3.67 * carbon_price_time_series
  
  mrv_cost[1] <- mrv_initial * regist_fee
  mrv_occur <- as.numeric(1:n_years%%round(mrv_recur)==0)
  mrv_cost[mrv_occur] <- annual_fee * mrv_recur + mrv_recurrent
  mrv_cost <- mrv_cost + afs_c_change * 3.67 * label_fee

  #Calculate the incoming cashflow derived from the intervention
  system_benefit <- n_hectares * (subsidy + AF_annual_benefit + walnuts_benefit + wood_benefit)
  cert_system_benefit <- system_benefit + n_hectares*carbon_benefit
  
  #Compute bottomline benefit of the intervention
  if (establishment_funded == TRUE) {
    establishment_cost[1] <- 0
  } else {
    establishment_cost[1] <- establishment_cost[1]
  }
  
  system_cost <- n_hectares * (establishment_cost + maintenance_cost + walnut_harvest_cost + tim_harvest_cost)
  bottomline_benefit <- system_benefit - system_cost
  
  cert_system_cost <- system_cost + mrv_cost
  cert_bottomline_benefit <- cert_system_benefit - cert_system_cost
  
  # Calculate the importance of each annual crop on the cashflow of the farm
  mono_wheat_benefit[c(seq(1, n_years, 3))] <- mono_annual_benefit[c(seq(1, n_years, 3))]
  mono_wheat_benefit[c(seq(2, n_years, 3))] <- 0
  mono_wheat_benefit[c(seq(3, n_years, 3))] <- 0
  AF_wheat_benefit[c(seq(1, n_years, 3))] <- AF_annual_benefit[c(seq(1, n_years, 3))]
  AF_wheat_benefit[c(seq(2, n_years, 3))] <- 0
  AF_wheat_benefit[c(seq(3, n_years, 3))] <- 0
  mono_barley_benefit[c(seq(1, n_years, 3))] <- 0
  mono_barley_benefit[c(seq(2, n_years, 3))] <- mono_annual_benefit[c(seq(2, n_years, 3))]
  mono_barley_benefit[c(seq(3, n_years, 3))] <- 0
  AF_barley_benefit[c(seq(1, n_years, 3))] <- 0
  AF_barley_benefit[c(seq(2, n_years, 3))] <- AF_annual_benefit[c(seq(2, n_years, 3))]
  AF_barley_benefit[c(seq(3, n_years, 3))] <- 0
  mono_rapeseed_benefit[c(seq(1, n_years, 3))] <- 0
  mono_rapeseed_benefit[c(seq(2, n_years, 3))] <- 0
  mono_rapeseed_benefit[c(seq(3, n_years, 3))] <- mono_annual_benefit[c(seq(3, n_years, 3))]
  AF_rapeseed_benefit[c(seq(1, n_years, 3))] <- 0
  AF_rapeseed_benefit[c(seq(2, n_years, 3))] <- 0
  AF_rapeseed_benefit[c(seq(3, n_years, 3))] <- AF_annual_benefit[c(seq(3, n_years, 3))]

  #Calculate the NPV of arable crop rotation without trees (i.e.: no intervention)
  NPV_mono <- discount(mono_annual_benefit, discount_rate = discount_rate, calculate_NPV = TRUE) 
  #Calculate the NPV of an alley cropping agroforestry iintervention with dual-purpose walnut trees
  NPV_system <- discount(bottomline_benefit, discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_cert_system <- discount(cert_bottomline_benefit, discount_rate = discount_rate, calculate_NPV = TRUE)
  #NPV of the decision to do an agroforestry intervention
  NPV_intervention <- NPV_system - NPV_mono
  NPV_cert_intervention <- NPV_cert_system - NPV_mono
  #NPV_tradeoff <- discount(tradeoff_benefit, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  #Expected difference of the NPV of wheat production between the bussines as usual and the agroforestry intervention
  NPV_mono_wheat <- discount(mono_wheat_benefit, discount_rate = discount_rate, calculate_NPV = TRUE) 
  NPV_AF_wheat <- discount(AF_wheat_benefit, discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_wheat <- NPV_AF_wheat - NPV_mono_wheat
  #Expected difference of the NPV of barley production between the bussines as usual and the agroforestry intervention
  NPV_mono_barley <- discount(mono_barley_benefit, discount_rate = discount_rate, calculate_NPV = TRUE) 
  NPV_AF_barley <- discount(AF_barley_benefit, discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_barley <- NPV_AF_barley - NPV_mono_barley
  #Expected difference of the NPV of rapeseed production between the bussines as usual and the agroforestry intervention
  NPV_mono_rapeseed <- discount(mono_rapeseed_benefit, discount_rate = discount_rate, calculate_NPV = TRUE) 
  NPV_AF_rapeseed <- discount(AF_rapeseed_benefit, discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_rapeseed <- NPV_AF_rapeseed - NPV_mono_rapeseed
   
  
  #Calculate the NPV of individual components of benefits of the intervention
  NPV_AF_arable_crops <- discount(AF_annual_benefit, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares
  NPV_walnuts <- discount(walnuts_benefit, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares
  NPV_wood <- discount(wood_benefit, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares
  NPV_carbon_offsetting <- discount(carbon_benefit, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares 
  NPV_AF_subsidy <- discount(subsidy, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares 
  
  NPV_design_planning_cost <- discount(design_cost, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares
  NPV_establishment_labor <- discount(establishment_labor, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares
  NPV_planting_cost <- discount(planting_cost, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares
  NPV_maintenance_cost <- discount(maintenance_cost, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares
  NPV_walnut_harvest_cost <- discount(walnut_harvest_cost, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares
  NPV_tim_harvest_cost <- discount(tim_harvest_cost, discount_rate = discount_rate, calculate_NPV = TRUE) * n_hectares
  NPV_mrv_cost <- discount(mrv_cost, discount_rate = discount_rate, calculate_NPV = TRUE)
  mean_annual_subsidy_benefit <- mean(maintenance_cost) - subsidy[n_years]
  
  return(list(NPV_treeless = NPV_mono, 
              NPV_AF = NPV_system,
              NPV_cert_AF = NPV_cert_system,
              NPV_decision_do = NPV_intervention, #The NPV of the Decision to adopt agroforestry
              NPV_decision_do_cert = NPV_cert_intervention,
              Cashflow_do_AF = bottomline_benefit - mono_annual_benefit,
              Cashflow_do_cert_AF = cert_bottomline_benefit - mono_annual_benefit,
              eCO2_offset_price_evolution = carbon_price_time_series,
              #we can also return the individual components of the costs and benefits to get an overview of their weight in the management of agroforestry systems
              #benefits
              NPV_arable_crops = NPV_AF_arable_crops, 
              NPV_nuts = NPV_walnuts, 
              NPV_timber_and_biomass = NPV_wood,
              NPV_financial_support = NPV_AF_subsidy, 
              NPV_carbon_marketing = NPV_carbon_offsetting,
              #costs
              NPV_planting_material_costs = NPV_planting_cost,
              NPV_establishment_labor_costs = NPV_establishment_labor,
              NPV_design_planning_costs = NPV_design_planning_cost,
              NPV_maintenance_labor_costs = NPV_maintenance_cost,
              NPV_walnut_harvest_costs = NPV_walnut_harvest_cost,
              NPV_mrv_costs = NPV_mrv_cost,
              NPV_tim_harvest_labor_costs = NPV_tim_harvest_cost,
              NPV_wheat_change = NPV_wheat,
              NPV_barley_change = NPV_barley,
              NPV_rapeseed_change = NPV_rapeseed,
              #net costs expressed as today´s value of money (multi-year variables expressed as annual average)
              planting_material_costs_today = planting_cost,
              establishment_labor_costs_today = establishment_labor)
              #desig_costs_today = design_planning,
              #mean_annual_maintenance_labor_costs = mean_annual_subsidy_benefit,
              #mean_annual_walnut_harvest_costs = mean(walnut_harvest_cost),
              #tim_harvest_labor_costs_today = tim_harvest_cost[n_years]),
              #final_timber_harvest_prices_today = final_harvest_benefit
              )
}

#Run the Monte Carlo analysis of the model
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv(input_file_name),
  model_function = AF_benefit,
  numberOfModelRuns = 1e4, #run 10,000 times
  functionSyntax = "plainNames")

#Plot the distributions of the NPV of the intervention and the non-intervention, to have a general visual perception on how they compare to each other
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_cert_AF", "NPV_treeless"),
                                    method = 'smooth_simple_overlay',
                                    old_names = c("NPV_cert_AF", "NPV_treeless"),
                                    new_names = c("AF intevention with CO2 certification", "Farming as usual"),
                                    x_axis_name = "NPV")+
  ggtitle("NPV of 100 hectares over 60 years")+
  #ggtitle("Carbon stock of in the aboveground biomass of a clear-cut agroforestry plot")+
  theme(plot.title = element_text(hjust = 0.5),
        #axis.text.y=element_blank(),
        #axis.text.x=element_blank(),
        #axis.line = element_line(arrow = arrow(type="open")),
        #panel.border = element_blank(),
        legend.position="bottom")
ggsave(
  filename = "NPV_cert_AF_vs_treeless.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"),
)

#Plot the distributions of the NPV of the intervention and the non-intervention, to have a general visual perception on how they compare to each other
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_AF", "NPV_treeless"),
                                    method = 'smooth_simple_overlay',
                                    old_names = c("NPV_AF", "NPV_treeless"),
                                    new_names = c("AF intevention without certification", "Farming as usual"),
                                    x_axis_name = "NPV")+
  ggtitle("NPV of 100 hectares over 60 years")+
  #ggtitle("Carbon stock of in the aboveground biomass of a clear-cut agroforestry plot")+
  theme(plot.title = element_text(hjust = 0.5),
        #axis.text.y=element_blank(),
        #axis.text.x=element_blank(),
        #axis.line = element_line(arrow = arrow(type="open")),
        #panel.border = element_blank(),
        legend.position="bottom")
ggsave(
  filename = "NPV_AF_vs_treeless.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"),
)

# This comparison can also be done with boxplots. This can be useful by illustrating the spread of the data resulting from the decision model. 
# Boxplots show the median (central line), the 25th and 75th percentiles (sides of boxes) and any outliers (light circles outside of boxes).
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_cert_AF", "NPV_AF", "NPV_treeless"),
                                    method = 'boxplot')
# Boxplots are also usefull to illustrate which individual components contribute the most to a certain outcome. In this case, benefit components are illustrated:
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_arable_crops", "NPV_nuts", "NPV_timber_and_biomass", "NPV_financial_support", "NPV_carbon_marketing"),
                                    old_names = c("NPV_arable_crops", "NPV_nuts", "NPV_timber_and_biomass", "NPV_financial_support", "NPV_carbon_marketing"),
                                    new_names = c("Sales of arable crops", "Sales of nuts", "Sales of woody biomass", "CAP's financial support", "Sales of carbon certificates"),
                                    y_axis_name = "",
                                    method = 'boxplot')+
  ggtitle("NPV of cash income variables (over 60 years) ")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(
  filename = "NPV_income_variables.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"),
)

#  and in this case, the costs components are illustrated:
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_planting_material_costs", "NPV_establishment_labor_costs", "NPV_maintenance_labor_costs", 
                                             "NPV_walnut_harvest_costs", "NPV_tim_harvest_labor_costs",
                                             "NPV_design_planning_costs", "NPV_mrv_costs"),
                                    old_names = c("NPV_planting_material_costs", "NPV_establishment_labor_costs", "NPV_maintenance_labor_costs", 
                                                  "NPV_walnut_harvest_costs", "NPV_tim_harvest_labor_costs",
                                                  "NPV_design_planning_costs", "NPV_mrv_costs"),
                                    new_names = c("Planting material", "Labor (establishment)", "Labor (maintenance)", 
                                                  "Nuts harvest and post-harvest", "Timber harvest and forwarding",
                                                  "Design and planning", "Certification costs"),
                                    y_axis_name = "",
                                    method = 'boxplot')+
  ggtitle("NPV of costs (over 60 years) ")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(
  filename = "NPV_costs.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"),
)


# We can also plot the value of the decision (difference in NPV between do and do not do). 
# This is more helpful for us since it shows us the outcome distribution of the decision itself.
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_do_cert",
                                    old_names = "NPV_decision_do_cert",
                                    new_names = "NPV (over 60 years) of the decision: do agroforestry intervention with carbon certification on 100 hectares",
                                    method = 'boxplot_density',
                                    y_axis_name = "Probability",
                                    x_axis_name = "Net decision outcome (NPV in Euro)")
ggsave(
  filename = "NPV_AF.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"),
)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_wheat_change",
                                    old_names = "NPV_wheat_change",
                                    new_names = "how is wheat-based income affected by the agroforestry intervention?",
                                    method = 'boxplot_density',
                                    y_axis_name = "Probability",
                                    x_axis_name = "NPV benefit of wheat sales in agroforestry vs. treeless cultivation")+
  xlim(-2000000,0)
ggsave(
  filename = "NPV_change_wheat.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"),
)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_barley_change",
                                    old_names = "NPV_barley_change",
                                    new_names = "how is barley-based income affected by the agroforestry intervention?",
                                    method = 'boxplot_density',
                                    y_axis_name = "Probability",
                                    x_axis_name = "NPV benefit of barley sales in agroforestry vs. treeless cultivation")+
  xlim(-2000000,0)
ggsave(
  filename = "NPV_change_barley.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"),
)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_rapeseed_change",
                                    old_names = "NPV_rapeseed_change",
                                    new_names = "how is rapeseed-based income affected by the agroforestry intervention?",
                                    method = 'boxplot_density',
                                    y_axis_name = "Probability",
                                    x_axis_name = "NPV benefit of rapeseed sales in agroforestry vs. treeless cultivation")+
  xlim(-2000000,0)
ggsave(
  filename = "NPV_change_rapeseed.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"),
)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_wheat_change", "NPV_barley_change", "NPV_rapeseed_change"),
                                    old_names = c("NPV_wheat_change", "NPV_barley_change", "NPV_rapeseed_change"),
                                    new_names = c("Wheat", "Barley", "Rapeseed"),
                                    method = 'boxplot',
                                    x_axis_name = "NPV outcome distribution",
                                    y_axis_name = "Arable crop")+
  ggtitle("NPV difference (over 60 years) of annual crop sales of the agroforestry intervention compared to no intervention")+
  theme(plot.title = element_text(hjust = 0.95))
ggsave(
  filename = "NPV_change_annual_crops.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"),
)
# Cashflow analysis
# Here we plot the distribution of annual cashflow over the entire simulated period for the intervention. 
decisionSupport::plot_cashflow(mcSimulation_object = mcSimulation_results, 
                               cashflow_var_name = "Cashflow_do_cert_AF", 
                               x_axis_name = "Timeline of the intervention (years)",
                               y_axis_name = "Cashflow (euros)")+
  ggtitle("Cashflow of 100 hectares of certified agroforestry intervention")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-500000,1000000)
ggsave(
  filename = "Cashflow.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"))

# Here we plot the distribution of annual cashflow over the entire simulated period for the intervention. 
decisionSupport::plot_cashflow(mcSimulation_object = mcSimulation_results, 
                               cashflow_var_name = "Cashflow_do_AF", 
                               x_axis_name = "Timeline of the intervention (years)",
                               y_axis_name = "Cashflow (euros)")+
  ggtitle("Cashflow of 100 hectares non-certified agroforestry intervention")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-500000,1000000)
ggsave(
  filename = "Cashflow_no_cert.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"))

decisionSupport::plot_cashflow(mcSimulation_object = mcSimulation_results, 
                               cashflow_var_name = "eCO2_offset_price_evolution", 
                               x_axis_name = "Timeline of the intervention (years)",
                               y_axis_name = "Euro per Mg of eCO2")+
  ggtitle("Price evolution of carbon certificates")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(
  filename = "Price_evolution_carbon_offset.png",
  plot = last_plot(),
  path = paste0(folder, "/", "Images_certification"))


##Conduct an analysis of the Expected Value of Perfect Information (EVPI)
#Run again the Monte Carlo analysis, this time returning output files
decisionSupport(inputFilePath = read.csv(input_file_name),
                outputPath = paste0(folder, "/", "MCResults", "/", "dual_purpose_walnut_rotation", sep=""),
                write_table = TRUE,
                welfareFunction = AF_benefit,
                numberOfModelRuns = 1e4, #run 10,000 times
                functionSyntax = "plainNames")

#The outcome table is then used to calculate the Value of Information for uncertain variables
#using the multi_EVPI function in the decisionSupport package (Luedeling, Goehring, and Schiﬀers 2019):
MCall <- read.table(file = paste0(folder, "/", "MCResults", "/", "dual_purpose_walnut_rotation", "/", "mcSimulationResults.csv"),
                    header = TRUE, sep=",")

#In order to calculate the Expected Value of Perfect Information (EVPI),
# it is necessary to extract a dataset that contains only the initial input variables and the output variable(s) of interest for the EVPI analysis
#ATTENTION: this must be done manually by the user
colnames(MCall)
mc <- MCall[,c(2:81,86)] #<- HERE change the indexed numbers to choose the pertinent columns (only necessary if you added or deleted variables in your input table, or if you added or deleted objects from the returned list of output variables)

#Calculate Expected Value of Perfect information
#In this case, the EVPI is performed for the NPV of the decision to intervene (transforming the arable field without trees into an alley cropping agroforestry system with dual-purpose walnut trees)
evpi_cert <- multi_EVPI(mc, "NPV_decision_do_cert") #, write_table = TRUE,
                   #outfolder = paste0(folder, "/", "EVPI", "/", "dual_purpose_walnut_wheat"))

# We use the function plot_evpi() on the results from multi_EVPI() to plot the Expected Value of Perfect Information (EVPI). 
# Here we show the results with the standard settings. The length of the bars is equal to EVPI.
plot_evpi(evpi_cert, decision_vars = "NPV_decision_do_cert")

# and the same for a non-certified project
mc <- MCall[,c(2:81,85)]
evpi <- multi_EVPI(mc, "NPV_decision_do") #, write_table = TRUE,
plot_evpi(evpi, decision_vars = "NPV_decision_do")
