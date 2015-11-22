year <- c(1:15)

typeval <- function(type) {
  switch(type,
         mkt = -1.099,
         org = -3.509)
}

ltvpv <- function(rev_member,disc,exp1) {
  pv <- rep(0,15)
  for (i in 1:15)   {
    pv[i] <- max(log(i) * exp1 + rev_member,0) / (1+(disc/100))^i
  }
  sum(pv)
}

ltv <- function(rev_member,exp1) {
  cf <- rep(0,15)  
  for (i in 1:15)   {
    cf[i] <- max(log(i) * exp1 + rev_member,0)
  }
  sum(cf)
}

cashflows <- function(rev_member,exp1) {
  cf <- rep(0,15)  
  for (i in 1:15)   {
    cf[i] <- max(log(i) * exp1 + rev_member,0)
  }
  cf
}


shinyServer(
  function(input, output) {
    output$netpermember <- renderPrint({input$netrev/input$members})
    output$ltvpv_pred <- renderPrint({ltvpv(input$netrev/input$members/(input$pct/100),input$discount,typeval(input$type))})
    output$ltv_pred <- renderPrint({ltv(input$netrev/input$members/(input$pct/100),typeval(input$type))})
    output$plot <- renderPlot({barplot(height=cashflows(input$netrev/input$members/(input$pct/100),typeval(input$type)),names.arg=year,main="Projected Future Cash Flows",xlab="Years",ylab="Net Revenue per Signup")})
  }
)