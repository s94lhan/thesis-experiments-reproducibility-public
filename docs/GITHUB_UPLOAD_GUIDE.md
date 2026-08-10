# Publishing the Repository on GitHub (Windows)

## 1. Complete the public-release checks

Before selecting public visibility:

1. Read `THIRD_PARTY_NOTICE.md`.
2. Confirm redistribution permission for the vendored `causalCalibration` snapshot, or replace it with an authorized dependency-installation method.
3. Select an appropriate license for the thesis author's original code. Do not add MIT, GPL, or another license without making that decision deliberately.
4. Confirm that the repository contains no personal names, student identifiers, credentials, machine-specific paths, or non-public thesis data.
5. Run `Rscript scripts/check_repository.R` from the repository root.

## 2. Create an empty GitHub repository

1. Sign in to GitHub and select `+` → `New repository`.
2. Use a name such as `thesis-experiments-reproducibility`.
3. Select **Public** only after completing the checks above.
4. Do not ask GitHub to create a README, `.gitignore`, or license because the local repository already contains the applicable release files.
5. Select `Create repository` and copy its HTTPS URL.

## 3. Publish with PowerShell

Open PowerShell and replace the example path and username below:

```powershell
Set-Location -LiteralPath "D:\path\to\thesis-experiments-reproducibility"
git init
git branch -M main
git status
git add .
git status
git commit -m "Initial reproducible thesis experiments"
git remote add origin https://github.com/YOUR-USERNAME/thesis-experiments-reproducibility.git
git push -u origin main
```

If Git reports that your identity is not configured, set it and repeat the commit:

```powershell
git config --global user.name "Your Name"
git config --global user.email "your-email@example.com"
```

GitHub does not accept an account password for Git pushes. Complete authentication through the browser, Git Credential Manager, or a personal access token.

## 4. Publish with GitHub Desktop

1. Open GitHub Desktop.
2. Select `File` → `Add local repository`.
3. Select the `thesis-experiments-reproducibility` folder.
4. If necessary, let GitHub Desktop create the local Git repository.
5. Enter a summary such as `Initial reproducible thesis experiments`.
6. Select `Commit to main`.
7. Select `Publish repository` and choose public visibility only after completing the release checks.

## 5. Later updates

Make future changes only in this release copy. Do not add the original large experiment directories to Git.

```powershell
Set-Location -LiteralPath "D:\path\to\thesis-experiments-reproducibility"
git status
git add .
git commit -m "Describe the update"
git push
```

## 6. Files that must remain untracked

The repository's ignore rules exclude:

- `runs/`: complete simulation outputs, per-observation CSV/RDS files, and model caches
- `cache/`: training and shared caches
- `renv/library/`: locally installed R packages
- `.Rhistory`, `.RData`, and `.Rproj.user/`

Do not use `git add -f` to add these paths. The complete main-experiment output is far larger than GitHub's per-file limit and does not belong in the source repository.

## 7. Verify a fresh clone

After publication, test the repository from a new directory:

```powershell
git clone https://github.com/YOUR-USERNAME/thesis-experiments-reproducibility.git
Set-Location thesis-experiments-reproducibility
Rscript scripts/check_repository.R
```

For a full reproduction, restore the locked environment separately in each experiment directory and follow the corresponding README.
