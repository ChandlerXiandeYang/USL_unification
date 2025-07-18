
# How to use these functions? 

## USL-Normalization
1) For one CQA: Ppu_KDEDPonUSLND generates the USL-normalized data's Ppu. Ppu_BAKDEDPonUSLND generates the Ppu and its 95% confidence interval with USL-normalized data.
2) For 1 to 3 CQAs: CQAWWC_KDEDPonUSLND generate CQA wise worst case Ppu for 1, 2, or 3 CQA DAR, CAR, Mic by USL-normalized data;CQAWWC_BAKDEDPonUSLND generate both Ppu and 95% confidence interval by bootstrap
3) For 1 to 3 CQAs, CQAWP_KDEDPonUSLND generate CQA wise pooled Ppu for 1, 2, or 3 CQA DAR, CAR, or Mic by USL-normalized data; CQAWP_BAKDEDPonUSLND generate both Ppu and 95% confidence interval by bootstrap
4) Monitoring Model 1: 1)+2)+CQAWWC_KDEDPonUSLND_CVStage3Monitoring
5) Monitoring Model 2: 1)+3)+CQAWWC_KDEDPonUSLND_CVStage3Monitoring

 Note:  Bootstrap 1000 iterations but if CIL<1, in the training stage, it will change to 10000 iterations. Bandwidth BW can be Silver1.06, Silver0.9, Silver0.9IQR, or be set by yourself. 
   
## Traditional Ppu  
7) For one CQA:
   Method 1: Split the data into several subgroup and use Ppu_KDEDP to generate the sugroup's Ppu by KDEDP (No USL-Normalization)
   Method 2: Ppu_SWWC_KDEDP generate the worst case Ppu  from all its subgroups. Note it uses Ppu_KDEDPonUSLND to calculate subgroup's Ppu because Ppu is an invariant (so using Ppu_KDEDP or Ppu_KDEDPonUSLND would generate the same output). This is the traditional method. it represents the CQA's Ppu.
   For 1 to 3 CQAs: Ppu_SWWC_KDEDP_overall generte the worst case Ppu for all CQA, i.e., it represent the overall Ppu of the cleaning provess.

## Bandwidth Function 

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
the value from my R code is exactly the same as in JMP.

## Data Permission 
Almost all functions here are used in the manuscript. The data here are simplified by removing critical information and then got approval from company. 



   
