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
                                     Adjusting_parameter){ # 0 <= Adjusting_parameter <=1 to adjust the kelly
  
  #***********
  # pre-errors
  # Adjusting_parameter
  if(any(Adjusting_parameter<0,
         Adjusting_parameter>1)){
    stop("Adjusting_parameter must be between 0 and 1")
  }
  
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
  Loss_cut_price=floor((Threshold/(n*100)+Filled_price)*100)/100
  
  #***********
  # post-errors
  # Loss_cut_price < Filled_price
  if(Loss_cut_price < Filled_price){
    stop("The estimated Loss_cut_price < Filled_price\nIt must be Loss_cut_price > Filled_price")
  }
  
  # loss_cut
  loss_cut=n*(Loss_cut_price-Filled_price)*100
  if(loss_cut>Capital){
    stop("loss_cut > Capital\n")
  }
  
  # original kelly
  kelly=p-(q*loss_cut)/profit
  
  # apply ap for new_kelly
  new_kelly=kelly*ap
  
  # # # inversely calculate new_loss_cut
  # (1)
  new_loss_cut=c*new_kelly
  
  # (2)
  # # new_kelly=p-(q*new_loss_cut)/profit
  # # new_kelly+(q*new_loss_cut)/profit=p
  # # q*new_loss_cut=(p-new_kelly)*profit
  # # new_loss_cut=((p-new_kelly)*profit)/q # new_loss_cut
  # new_loss_cut=((p-new_kelly)*profit)/q # new_loss_cut
  
  # new_Loss_cut_price
  # new_loss_cut=n*(new_Loss_cut_price-Filled_price)*100
  # new_Loss_cut_price=new_loss_cut/(n*100)+Filled_price
  new_Loss_cut_price=new_loss_cut/(n*100)+Filled_price # final
  
  # out_list
  out_list=list(
    Margin=Margin,
    
    kelly=new_kelly,
    
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
      loss_cut=new_loss_cut,
      Loss_cut_price=new_Loss_cut_price
    )
  )
  
  return(out_list)
}

# test
Output_list=
  Loss_cut_price_determinator(
    Capital=100000,
    Strike_price=100,
    Delta=0.3,
    Filled_price=1.5,
    Profit_price=0.5,
    N_of_contracts=10,
    Adjusting_parameter=1
  )
Output_list

# double-check
p=(1-Output_list$input$Delta)
q=1-p
loss_cut=Output_list$output$loss_cut
profit=Output_list$output$profit
kelly=Output_list$kelly

kelly*Output_list$input$Capital
