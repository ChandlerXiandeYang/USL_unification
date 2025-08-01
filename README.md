#  1. How to Use the Code to Verify the Output in the Manuscript

The file `CODE_AND_its_output_for_the_manuscript.qmd` contains all the code calculating Ppu used in the manuscript titled:

**"Specification Limit Normalization for Nonparametric Process Capability Analysis and Its Application in Cleaning Validation with AI-Enabled Monitoring Models."**

### Steps to Reproduce the Output

1. Download the data from the folder named `"Equipment A"`.
2. Download the file `CODE_AND_its_output_for_the_manuscript.qmd`.
3. Update the file path in the `.qmd` file to reflect the location where you saved the data.
4. Run the file in RStudio or use the `quarto::quarto_render()` function in R.
5. The output will include **15 tables**, labeled Table 1 to Table 15.

> **Note:** The tables in the manuscript are numbered one higher than those generated (i.e., Table 1 in the code corresponds to Table 2 in the manuscript).  
> The file may take **up to 1.5 hours** to run due to more than **2,000 bootstrap iterations**.


---

#  2. How to Use the Functions in Business Operations

This section is for practitioners who want to understand or apply the functions in business operations. All functions are located in the folder `"Functions"`.

## 2.1 USL-Normalization Method to Calculate Ppu for a CQA or a Cleaning Process

### 2.1.1 To Assess One of DAR, CAR, or Mic Ppu Using `Ppu_KDEDPonUSLND` Method
- `Ppu_KDEDPonUSLND`: Calculates Ppu using USL-normalized data.
- `Ppu_BAKDEDPonUSLND`: Calculates Ppu and its 95% confidence interval using bootstrap.

### 2.1.2 To Assess Overall Cleaning Process Ppu (1–3 CQAs: DAR, CAR, Mic) Using CQAWWC Method
- `CQAWWC_KDEDPonUSLND`: Computes CQA-wise worst-case Ppu.
- `CQAWWC_BAKDEDPonUSLND`: Computes Ppu and 95% CI using bootstrap.

### 2.1.3 To Assess Overall Cleaning Process Ppu (1–3 CQAs: DAR, CAR, Mic) Using CQAWP Method
- `CQAWP_KDEDPonUSLND`: Computes pooled Ppu across CQAs.
- `CQAWP_BAKDEDPonUSLND`: Computes pooled Ppu and 95% CI using bootstrap.

### 2.1.4 To Monitor/Verify/Test a Cleaning Process for Stage 3 CPV
- **Model 1**: Combines 2.1.1 + 2.1.2 + `CQAWWC_BAKDEDPonUSLND_CVStage3Monitoring`
- **Model 2**: Combines 2.1.1 + 2.1.3 + `CQAWP_BAKDEDPonUSLND_CVStage3Monitoring`

> **Note:** Bootstrap uses 1,000 iterations by default. If the confidence interval lower limit (CIL) is less than 1 during training, it switches to 10,000 iterations.  
> Bandwidth (`BW`) options include: `Silver1.06`, `Silver0.9`, `Silver0.9IQR`, or user-defined.

---

## 2.2 Traditional Method to Calculate Ppu for a CQA or a Cleaning Process

### 2.2.1 To Assess One of DAR, CAR, or Mic Ppu Using `Ppu_KDEDP` Method
- **Method 1**: Manually split data into subgroups, use `Ppu_KDEDP` to calculate each subgroup's Ppu (no USL normalization), and take the minimum. This is the traditional manual method.
- **Method 2**: Use `Ppu_SWWC_KDEDP` to compute worst-case Ppu across subgroups. No need to manually split data—the function detects subgroups automatically.

> **Note:** `Ppu_SWWC_KDEDP` uses `Ppu_KDEDPonUSLND` internally, as Ppu is invariant to USL normalization.  
> You can replace `Ppu_KDEDPonUSLND` with `Ppu_KDEDP` in `Ppu_SWWC_KDEDP`—the output remains the same since Ppu, Ppl, and Ppk are USL-normalization invariants.

### 2.2.2 To Assess Overall Cleaning Process Ppu by Worst Case Across 1–3 CQAs: DAR, CAR, Mic
- `Ppu_SWWC_KDEDP_overall`: Computes worst-case Ppu across all CQAs, representing overall cleaning process capability.

---


##  3. Bandwidth Function Clarification

Although **JMP** claims its bandwidth formula is BW = 0.9 * s / n^(1/5)

Where:

- `s` is the **uncorrected** sample standard deviation (divided by `n`, not `n - 1`)
- Grid size is set to 100

> **However, this is not accurate** based on our findings.

We observed that:

- The actual bandwidth formula should be BW = 1.06 * s / n^(1/5)

  where `s` is the **corrected** sample standard deviation (divided by `n - 1`)
  
- The grid size should be **much larger** than 100. We set the grid size=2^15.

  This adjustment provides more accurate density estimation and better alignment with theoretical expectations.
---

All functions in the code allow manual input of the bandwidth `h`, as our code already accounts for this flexibility.

```r
 if (is.character(BW)) {
    BW <- match.arg(BW, choices = c("Silver1.06", "Silver0.9", "Silver0.9IQR"))
    h <- switch(BW,
                "Silver1.06" = 1.06 * s / n^(1/5),
                "Silver0.9" = 0.9 * s / n^(1/5),
                "Silver0.9IQR" = {
                  iqr_val <- IQR(x)
                  sigma <- min(s, iqr_val / 1.34)
                  0.9 * sigma / n^(1/5)
                })
    bw_method <- BW
  } else if (is.numeric(BW) && length(BW) == 1 && BW > 0) {
    h <- BW
    bw_method <- "User-defined"
  } else {
    stop("BW must be numeric > 0 or one of: 'Silver1.06', 'Silver0.9', 'Silver0.9IQR'.")
  }
```

We will revise the R package "CleaningValidation" once all methodologies are recognized by practitioners and regulatory authorities.
