#********************
#
# empty the workspace
#
#********************
rm(list=ls())

library(data.table)
Loss_cut_price_determinator=function(Capital,
                                     Strike_price,
                                     Delta,
                                     Filled_price, # Bid (=Premium price)
                                     Profit_price, # Filled_price > Profit_price
                                     N_of_contracts,
                                     Adjusting_parameter){ # 0 <= Adjusting_parameter <=1 to adjust the kelly; not working now
  
  #***********
  # pre-errors
  # Adjusting_parameter
  # if(any(Adjusting_parameter<0,
  #        Adjusting_parameter>1)){
  #   stop("Adjusting_parameter must be between 0 and 1")
  # }
  
  # excess of margin 
  Margin=Strike_price*N_of_contracts*100*0.45
  if(Margin>Capital){
    stop(paste0("The estimated Margin > Capital\n  Maximum contract N = ", floor(Capital/(Strike_price*100*0.45))))
  }
  
  # Filled_price < Profit_price
  if(Filled_price < Profit_price){
    stop("The estimated Filled_price < Profit_price\nIt must be Filled_price > Profit_price")
  }
  
  #***********************
  # induce the formula (b)
  #***********************
  # # p = probability to profit (1 - Delta, since it's short)
  # # q = 1 - p
  # # n = number of contract
  # kelly=p-q/b
  # kelly=p-(q*loss_cut)/profit
  # 
  # c*kelly=loss_cut # (a), 0 <= (a) <= c
  # kelly=loss_cut/c=p-(q*loss_cut)/profit
  # p-(q*loss_cut)/profit>=loss_cut/c
  # p>=loss_cut/c+(q*loss_cut)/profit
  # p>=loss_cut*(1/c+q/profit)
  # p/(1/c+q/profit)>=loss_cut # however, the raw kelly is risky, so we'll consider an adjusting parameter, ap.
  # p/(1/c+q/profit)>=loss_cut # final form, (b)
  
  # Capital=100000
  # Strike_price=100
  # Delta=0.3
  # Filled_price=1.5
  # Profit_price=0.5
  # N_of_contracts=10
  # Adjusting_parameter=0.5
  
  # variables
  p=1-Delta
  n=N_of_contracts
  c=Capital
  q=1-p
  ap=Adjusting_parameter
  
  #********************
  # since this is short,
  # Filled_price > Profit_price
  # Loss_cut_price > Filled_price
  profit=n*(Filled_price-Profit_price)*100 # 100 underlying stocks
  # loss_cut=n*(Loss_cut_price-Filled_price)*100
  
  # Since profit and loss_cut are associated as follows, either can be determined from the other
  # p/(1/c+q/profit)>=loss_cut # (b)
  Threshold=p/(1/c+q/profit)
  # -> Threshold>=loss_cut
  # -> Threshold>=n*(Loss_cut_price-Filled_price)*100
  # (floor) Loss_cut_price=floor((Threshold/(n*100)+Filled_price)*100)/100
  Loss_cut_price=Threshold/(n*100)+Filled_price
  
  # loss_cut
  loss_cut=n*(Loss_cut_price-Filled_price)*100
  
  # adjusted_applied_kelly
  adjusted_applied_kelly=(p-(q*loss_cut)/profit)*ap
  
  # ap_loss_cut
  # -> adjusted_applied_kelly=ap_loss_cut/c
  # -> ap_loss_cut=adjusted_applied_kelly*c
  ap_loss_cut=adjusted_applied_kelly*c
  
  # ap_Loss_cut_price
  # ap_loss_cut=n*(ap_Loss_cut_price-Filled_price)*100
  # -> ap_Loss_cut_price=ap_loss_cut/(100*n)+Filled_price
  # (floor) ap_Loss_cut_price=floor((ap_loss_cut/(100*n)+Filled_price)*100)/100
  ap_Loss_cut_price=(ap_loss_cut/(100*n)+Filled_price)
  
  # ap_loss_cut
  ap_loss_cut=n*(ap_Loss_cut_price-Filled_price)*100
  
  # adjusted_theoretical_kelly
  # adjusted_theoretical_kelly != adjusted_applied_kelly
  # adjusted_theoretical_kelly is an actual adjusted kelly
  # adjusted_theoretical_kelly must be larger than adjusted_applied_kelly ( adjusted_theoretical_kelly > adjusted_applied_kelly )
  adjusted_theoretical_kelly=p-(q*ap_loss_cut)/profit
  
  #***********
  # post-errors
  # ap_Loss_cut_price < Filled_price
  if(ap_Loss_cut_price < Filled_price){
    stop("The estimated ap_Loss_cut_price < Filled_price\nIt must be ap_Loss_cut_price > Filled_price")
  }
  
  # ap_loss_cut
  if(ap_loss_cut > Capital){
    stop("ap_loss_cut > Capital\n")
  }
  
  # adjusted_theoretical_kelly
  if(round(adjusted_theoretical_kelly, 8) < round(adjusted_applied_kelly, 8)){
    stop("adjusted_theoretical_kelly < adjusted_applied_kelly\n")
  }
  
  # out_list
  out_list=list(
    Margin=Margin,
    
    # kelly=(p-(q*loss_cut)/profit),
    adjusted_theoretical_kelly=adjusted_theoretical_kelly,
    
    # adjusted_applied_kelly
    adjusted_applied_kelly=adjusted_applied_kelly,
    
    input=data.table(
      Capital=Capital,
      Strike_price=Strike_price,
      Delta=Delta,
      Filled_price=Filled_price,
      Profit_price=Profit_price,
      N_of_contracts=N_of_contracts,
      Adjusting_parameter=Adjusting_parameter
    ),
    
    output=data.table(
      profit=n*(Filled_price-Profit_price)*100,
      # loss_cut=loss_cut,
      # Loss_cut_price=Loss_cut_price
      loss_cut=ap_loss_cut,
      Loss_cut_price=ap_Loss_cut_price
    )
  )
  
  return(out_list)
}

# test
Output_list=
  Loss_cut_price_determinator(
    Capital=100000,
    Strike_price=62,
    Delta=0.486,
    Filled_price=3.67,
    Profit_price=2,
    N_of_contracts=5,
    Adjusting_parameter=0.5
  )
Output_list

# double-check
p=(1-Output_list$input$Delta)
q=1-p
loss_cut=Output_list$output$loss_cut
profit=Output_list$output$profit
kelly=Output_list$adjusted_applied_kelly

kelly*Output_list$input$Capital

Output_list$output$Loss_cut_price-Output_list$input$Filled_price
Output_list$input$Filled_price-Output_list$input$Profit_price
