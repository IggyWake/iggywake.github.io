---
#
# By default, content added below the "---" mark will appear in the home page
# between the top bar and the list of recent posts.
# To change the home page layout, edit the _layouts/home.html file.
# See: https://jekyllrb.com/docs/themes/#overriding-theme-defaults
#
layout: home
---
<style>
  /* CSS to create the grid and style the cards */
  .portfolio-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 20px;
    margin-top: 20px;
  }
  .project-card {
    border: 1px solid #e1e4e8;
    border-radius: 8px;
    padding: 20px;
    background-color: #ffffff;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  .project-title {
    margin-top: 0;
    margin-bottom: 10px;
    font-size: 1.25em;
  }
  .tag-container {
    margin-bottom: 15px;
  }
  .tag {
    display: inline-block;
    background-color: #f1f8ff;
    color: #0366d6;
    padding: 4px 8px;
    border-radius: 12px;
    font-size: 0.8em;
    margin-right: 5px;
    margin-bottom: 5px;
  }
  .btn-primary {
    display: inline-block;
    background-color: #24292e;
    color: #ffffff !important; /* Forces text to be white, ignoring theme defaults */
    padding: 8px 14px;
    border-radius: 6px;
    text-decoration: none;
    font-size: 0.9em;
    margin-top: 10px;
    margin-right: 10px;
  }
  .btn-primary:hover {
    background-color: #000000;
  }
  .btn-secondary {
    display: inline-block;
    background-color: #fafbfc;
    color: #24292e !important;
    border: 1px solid #d1d5da;
    padding: 8px 14px;
    border-radius: 6px;
    text-decoration: none;
    font-size: 0.9em;
    margin-top: 10px;
  }
  .btn-secondary:hover {
    background-color: #f3f4f6;
  }
</style>

<h2>Featured Projects</h2>

<div class="portfolio-grid">

  <!-- Project 1: Dark Souls -->
  <div class="project-card">
    <h3 class="project-title">Dark Souls Item Description Scraper</h3>
    <div class="tag-container">
      <span class="tag">Python</span>
      <span class="tag">Web Scraping</span>
      <span class="tag">BeautifulSoup</span>
    </div>
    <p>An automated web scraper that extracts and structures lore and item descriptions from the Dark Souls wiki into a clean dataset for analysis.</p>
    <a href="URL_TO_YOUR_GITHUB_REPO" class="btn-primary" target="_blank">View Code ➔</a>
    <a href="PATH_TO_YOUR_FILE.csv" class="btn-secondary" download>Get CSV File ⬇</a>
  </div>

  <!-- Project 2: NetLogo -->
  <div class="project-card">
    <h3 class="project-title">Evolutionary Spread of Altruist Traits</h3>
    <div class="tag-container">
      <span class="tag">NetLogo</span>
      <span class="tag">Mathematical Modeling</span>
      <span class="tag">Research</span>
    </div>
    <p>An agent-based simulation mapping the organic complexity of altruistic behaviors in populations into a functional mathematical model.</p>
    <a href="URL_TO_YOUR_GITHUB_REPO" class="btn-primary" target="_blank">View Code ➔</a>
    <a href="PATH_TO_YOUR_PAPER.pdf" class="btn-secondary" download>Get Paper (PDF) ⬇</a>
  </div>

  <!-- Project 3: Cibervoluntarios -->
  <div class="project-card">
    <h3 class="project-title">Fundación Cibervoluntarios Data Report</h3>
    <div class="tag-container">
      <span class="tag">Data Analysis</span>
      <span class="tag">Reporting</span>
      <span class="tag">Solo Authorship</span>
    </div>
    <p>An independent, comprehensive data report analyzing operational metrics and community impact, translating real-world systems into actionable insights.</p>
    <a href="PATH_TO_YOUR_REPORT.pdf" class="btn-primary" download>Get Report (PDF) ⬇</a>
  </div>

</div>
