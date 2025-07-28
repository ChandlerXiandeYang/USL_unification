# 📘 1 How to Use the Code to Verify the Output in the Manuscript

The file `CODE_AND_its_output_for_the_manuscript.qmd` contains all the code used in the manuscript titled:

**"Specification Limit Normalization for Nonparametric Process Capability Analysis and Its Application in Cleaning Validation with AI-Enabled Monitoring Models."**


##  Steps to Reproduce the Output

1). **Download the data** from the folder named **"Equipment A"**

2). **Download the file** `CODE_AND_its_output_for_the_manuscript.qmd`

3). **Update the file path** in the `.qmd` file to reflect the location where you saved the data

4). **Run the file** in RStudio or use the `quarto::quarto_render()` function in R.

5). The output will include **15 tables** labeled Table 1 to Table 15.  

   > **Note:** The tables in the manuscript are numbered one higher than those generated (i.e., Table 1 in the code corresponds to Table 2 in the manuscript). "CODE_AND_its_output_for_the_manuscript.qmd" can spend 1.5 hours to get the output because it totally runs more than 2000 bootstrap iterations.
 
# **If you just verify the output or the code, use `CODE_AND_its_output_for_the_manuscript.qmd` and `CODE_AND_its_output_for_the_manuscript.pdf` only.**

---

# ⚠️ For Practitioners Only

# 🧠 2. How to Use the Functions in Business Operations

This section is for users who want to understand or apply the functions in business operations. All functions are located in the folder `"Functions"`.

## 2.1 USL-Normalization Functions

### 2.1.1 Single CQA Using `Ppu_KDEDPonUSLND` Method:
- `Ppu_KDEDPonUSLND`: Calculates Ppu using USL-normalized data.
- `Ppu_BAKDEDPonUSLND`: Calculates Ppu and its 95% confidence interval using bootstrap.

### 2.1.2 Worst-Case Ppu for 1–3 CQAs (DAR, CAR, Mic):
- `CQAWWC_KDEDPonUSLND`: Computes CQA-wise worst-case Ppu.
- `CQAWWC_BAKDEDPonUSLND`: Computes Ppu and 95% CI using bootstrap.

### 2.1.3 Pooled Ppu for 1–3 CQAs (DAR, CAR, Mic):
- `CQAWP_KDEDPonUSLND`: Computes pooled Ppu across CQAs.
- `CQAWP_BAKDEDPonUSLND`: Computes pooled Ppu and 95% CI using bootstrap.

### 2.1.4 Monitoring Models:
- **Model 1**: Combines 2.1.1 + 2.1.2 + `CQAWWC_KDEDPonUSLND_CVStage3Monitoring`
- **Model 2**: Combines 2.1.1 + 2.1.2 + `CQAWWC_KDEDPonUSLND_CVStage3Monitoring`

> **Note:** Bootstrap uses 1,000 iterations by default. If the confidence interval lower limit (CIL) is less than 1 during training, it switches to 10,000 iterations.  
> Bandwidth (`BW`) options include: `Silver1.06`, `Silver0.9`, `Silver0.9IQR`, or user-defined.

---

## 2.2 Traditional Ppu Methods

### 2.2.1 For a Single CQA:
- **Method 1**: Split data into subgroups and use `Ppu_KDEDP` (no USL normalization). This is the traditional method.
- **Method 2**: Use `Ppu_SWWC_KDEDP` to compute worst-case Ppu across subgroups.

> **Note:** `Ppu_SWWC_KDEDP` uses `Ppu_KDEDPonUSLND` internally, as Ppu is invariant to USL normalization.  
> You can replace `Ppu_KDEDPonUSLND` with `Ppu_KDEDP` in `Ppu_SWWC_KDEDP`—the output remains the same since Ppu, Ppl, and Ppk are USL-normalization invariants.

### 2.2.2 For 1–3 CQAs:
- `Ppu_SWWC_KDEDP_overall`: Computes worst-case Ppu across all CQAs, representing overall cleaning process capability.

---

## 📐 3. Bandwidth Function Clarification

Although **JMP** claims its bandwidth formula is:

