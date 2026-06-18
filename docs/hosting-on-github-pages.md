# Hosting on GitHub Pages

Run `feed-repeat` on GitHub Actions and Pages: fork this repo, edit the config, and let
GitHub Actions publish your repeated feeds to GitHub Pages.

## Quick Start

1. Fork this repository on GitHub.
2. Edit [`config.yaml`](https://github.com/abhin4v/feed-repeat/blob/main/config.yaml): replace the example tasks with the source feeds you want repeated and the output filenames you want them served under.
3. [Set up the GitHub Action](#setting-up-the-github-action).
4. [Set up GitHub Pages](#setting-up-github-pages).
5. Push your changes. The action runs daily and republishes the feeds.

## Setting up the GitHub Action

The workflow at [`.github/workflows/feeds.yml`](https://github.com/abhin4v/feed-repeat/blob/main/.github/workflows/feeds.yml) runs every six hours, on every push to `main`, and on manual dispatch. It needs permission to push to a `gh-pages` branch.

1. In your fork, go to **Settings → Actions → General**.
2. Under **Workflow permissions**, select **Read and write permissions** and click **Save**. This lets the workflow create and update the `gh-pages` branch.
3. Go to the **Actions** tab. If Actions are disabled on your fork, click _I understand my workflows, go ahead and enable them_.
4. Open the **feed-repeat** workflow and click **Enable workflow**.
5. Click **Run workflow** to trigger the first build manually. The schedule takes over after that.
6. Wait for the run to finish. It will create a `gh-pages` branch in your fork containing the generated Atom files.

## Setting up GitHub Pages

After the first successful Action run has created the `gh-pages` branch:

1. Go to **Settings → Pages**.
2. Under **Build and deployment**, set **Source** to **Deploy from a branch**.
3. Set **Branch** to `gh-pages` and the folder to `/ (root)`. Click **Save**.
4. Wait a minute, then refresh the Pages settings page. GitHub will show the public URL, e.g. `https://<you>.github.io/<repo>/`.
5. Your feeds are served at `https://<you>.github.io/<repo>/<outputFilename>.atom`. For example, with the default config: `https://<you>.github.io/feed-repeat/a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6.atom`.

If you use a custom domain, add a `CNAME` file to the repo root containing your domain (the workflow copies it into `_site/` so it lands at the root of the `gh-pages` branch), and configure the domain under **Settings → Pages → Custom domain**. Without the `CNAME` file in the published branch, GitHub clears the custom-domain setting on every deploy.

## Caveats

- Schedule can be delayed. GitHub's hosted runners don't guarantee cron timing. The schedule may run several minutes after the hour.
- 60-day inactivity disable. GitHub disables scheduled workflows on repositories with no activity for 60 days. Click *Run workflow* periodically to keep your fork active.
- First run is slow. The workflow has to download and compile the tool from scratch. Subsequent runs use the cached build, so they take seconds.
