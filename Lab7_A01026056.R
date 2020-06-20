hypothesisTest_z = function(test, alpha, mu0, n, xbar, sigma) {
  z_test = (xbar-mu0)/(sigma/sqrt(n))
  if (test=="upper") {
    p_value = 1-pnorm(z_test)
    z_critical = qnorm(1-alpha)
    cat("H0: mu =", mu0, " Ha: mu > ", mu0, "\n")
    cat("z_test = ", z_test, " z_critical = ", z_critical, "\n")
    cat("p_value = ", p_value, " alph = ",alpha, "\n")
    if(z_test > z_critical) {
      cat ("Reject H0 since z_test > z_critical (or p_value < alpha)")
    }else {
      cat("Fail to reject H0 since z_test < z_critical (or p_value > alpha)")
    }
  } else if (test == "lower") {
    #lower-tailed test
    p_value = pnorm(z_test)
    z_critical = qnorm(alpha)
    cat("H0: mu =", mu0, " Ha: mu < ", mu0, "\n")
    cat("z_test = ", z_test, " z_critical = ", z_critical, "\n")
    cat("p_value = ", p_value, " alph = ",alpha, "\n")
    if(z_test < z_critical) {
      cat ("Reject H0 since z_test < z_critical (or p_value > alpha)")
    }else {
      cat("Fail to reject H0 since z_test > z_critical (or p_value > alpha)")
    }  
  } else if (test=="twotailed") {
    p_value = pnorm(z_test)
    z_critical = qnorm(alpha)
    cat("H0: mu =", mu0, " Ha: mu < ", mu0, "\n")
    cat("z_test = ", z_test, " z_critical = ", z_critical, "\n")
    cat("p_value = ", p_value, " alph = ",alpha, "\n")
    if(z_test != z_critical) {
      cat ("Reject H0 since z_test = z_critical (or p_value > alpha)")
    }else {
      cat("Fail to reject H0 since z_test != z_critical (or p_value > alpha)")
    }  
  }else {
    print("Error!")
  }
}
hypothesisTest_t = function(test, alpha, mu0, n, xbar, sigma) {
  z_test = (xbar-mu0)/(sigma/sqrt(n))
  if (test=="upper") {
    p_value = pt(alpha, df=n-1)
    z_critical = qt(alpha, df=n-1) 
    cat("H0: mu =", mu0, " Ha: mu > ", mu0, "\n")
    cat("z_test = ", z_test, " z_critical = ", z_critical, "\n")
    cat("p_value = ", p_value, " alph = ",alpha, "\n")
    if(z_test > z_critical) {
      cat ("Reject H0 since z_test > z_critical (or p_value < alpha)")
    }else {
      cat("Fail to reject H0 since z_test < z_critical (or p_value > alpha)")
    }
  } else if (test == "lower") {
    #lower-tailed test
    p_value = pnorm(z_test)
    z_critical = qnorm(alpha)
    cat("H0: mu =", mu0, " Ha: mu < ", mu0, "\n")
    cat("z_test = ", z_test, " z_critical = ", z_critical, "\n")
    cat("p_value = ", p_value, " alph = ",alpha, "\n")
    if(z_test < z_critical) {
      cat ("Reject H0 since z_test < z_critical (or p_value > alpha)")
    }else {
      cat("Fail to reject H0 since z_test > z_critical (or p_value > alpha)")
    }  
  } else if (test=="twotailed") {
    p_value = pnorm(z_test)
    z_critical = qnorm(alpha)
    cat("H0: mu =", mu0, " Ha: mu < ", mu0, "\n")
    cat("z_test = ", z_test, " z_critical = ", z_critical, "\n")
    cat("p_value = ", p_value, " alph = ",alpha, "\n")
    if(z_test != z_critical) {
      cat ("Reject H0 since z_test = z_critical (or p_value > alpha)")
    }else {
      cat("Fail to reject H0 since z_test != z_critical (or p_value > alpha)")
    }  
  }else {
    print("Error!")
  }
}
hypothesisTest_z("upper", 0.05, 8, 50, 8.3, 2.0)
hypothesisTest_z("lower", 0.05, 8, 50, 7.3, 2.0)
hypothesisTest_z("lower", 0.05, 8, 50, 7.7, 2.0)
hypothesisTest_z("twotailed", 0.05, 8, 50, 8.3, 2.0)
hypothesisTest_z("twotailed", 0.05, 8, 50, 7.3, 2.0)

hypothesisTest_t("lower", 0.05, 8, 50, 9.5, 2.0)
hypothesisTest_t("twotailed", 0.05, 8, 50, 6.0, 2.0)
hypothesisTest_t("upper", 0.05, 8, 50, 6.0, 2.0)