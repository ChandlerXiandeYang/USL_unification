# 📘 How to Use the Code to Verify the Output in the Manuscript

The file `Code_and_its_output_for_the_manuscript.qmd` contains all the code used in the manuscript titled:

**"Specification Limit Normalization for Nonparametric Process Capability Analysis and Its Application in Cleaning Validation with AI-Enabled Monitoring Models."**

## 🔧 Steps to Reproduce the Output

1. **Download the data** from the folder named **"Equipment A."**
2. **Download the file** `Code_and_its_output_for_the_manuscript.qmd`.
3. **Update the file path** in the `.qmd` file to reflect the location where you saved the data.
4. **Run the file** in RStudio or use the `quarto::quarto_render()` function in R.
5. The output will include **15 tables** labeled Table 1 to Table 15.  
   > **Note:** The tables in the manuscript are numbered one higher than those generated (i.e., Table 1 in the code corresponds to Table 2 in the manuscript).

---

# 🧠 How to Use the Functions

This section is for users who want to understand the functions or apply them in business operations.

## 📏 USL-Normalization Functions

### For a single CQA:
- `Ppu_KDEDPonUSLND`: Calculates Ppu using USL-normalized data.
- `Ppu_BAKDEDPonUSLND`: Calculates Ppu and its 95% confidence interval using bootstrap.

### For 1 to 3 CQAs (DAR, CAR, Mic):
- `CQAWWC_KDEDPonUSLND`: Computes CQA-wise worst-case Ppu.
- `CQAWWC_BAKDEDPonUSLND`: Computes Ppu and 95% CI using bootstrap.

### CQA-wise Pooled Ppu:
- `CQAWP_KDEDPonUSLND`: Computes pooled Ppu for 1–3 CQAs.
- `CQAWP_BAKDEDPonUSLND`: Computes pooled Ppu and 95% CI using bootstrap.

### Monitoring Models:
- **Model 1**: Combines (1) + (2) + `CQAWWC_KDEDPonUSLND_CVStage3Monitoring`
- **Model 2**: Combines (1) + (3) + `CQAWWC_KDEDPonUSLND_CVStage3Monitoring`

> **Note:** Bootstrap uses 1,000 iterations by default. If the confidence interval length (CIL) is less than 1 during training, it switches to 10,000 iterations.  
> Bandwidth (`BW`) options include: `Silver1.06`, `Silver0.9`, `Silver0.9IQR`, or user-defined.

---

## 📊 Traditional Ppu Methods

### For a single CQA:
- **Method 1**: Split data into subgroups and use `Ppu_KDEDP` (no USL normalization).
- **Method 2**: Use `Ppu_SWWC_KDEDP` to compute worst-case Ppu across subgroups.  
  > Note: This uses `Ppu_KDEDPonUSLND` internally, as Ppu is invariant to normalization.

### For 1 to 3 CQAs:
- `Ppu_SWWC_KDEDP_overall`: Computes worst-case Ppu across all CQAs, representing the overall cleaning process capability.

---

## 📐 Bandwidth Function Clarification

Although JMP claims its bandwidth formula is:




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

Enjoy the code and the logic!!!
