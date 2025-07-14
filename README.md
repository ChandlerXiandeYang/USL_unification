 Bandwidth Function

JMP says its bandwidth is bw=0.9s/n^{1/5} and s is the uncoorrected sample standard deviation (i.e. divided by n but not n-1), and the grid=100.
However, it is not the case. I found bw=1.06s/n^{1/5} and s is the corrected sample standard deviation, and the grid should be much larger than grid=100. So, I used 2^15.

h <- switch(BW,
                "Silver1.06" = 1.06 * s / n^(1/5),
                "Silver0.9" = 0.9 * s / n^(1/5),
                "Silver0.9IQR" = {
                  iqr_val <- IQR(x)
                  sigma <- min(s, iqr_val / 1.34)
                  0.9 * sigma / n^(1/5)
                })


All function's h can be input by manually. If you want to compare with JMP or others, you can manually input. For example, if we input h=2.8809, then for Equipment DAR,
the value from my R coe is exactly the same as in JMP.

Almost all functions here are used in the manuscript. The data here are simplified by removing critical information and had got approval from company. Only basice values are  kept here.
