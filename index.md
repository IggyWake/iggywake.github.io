---
layout: home
---

<style>

  /* Profile Header Styles */
  .profile-header {
    display: flex;
    align-items: center;
    gap: 30px;
    margin-bottom: 50px;
    margin-top: 20px;
  }
  .profile-pic {
    width: 140px;
    height: 140px;
    border-radius: 50%;
    object-fit: cover;
    border: 3px solid #e1e4e8;
    box-shadow: 0 4px 6px rgba(0,0,0,0.05);
  }
  .profile-name {
    font-size: 2.5em;
    font-weight: 300;
    margin: 0 0 5px 0;
    color: #24292e;
    letter-spacing: -1px;
  }
  .profile-role {
    font-size: 1.1em;
    font-weight: 600;
    color: #586069;
    margin: 0;
    text-transform: uppercase;
    letter-spacing: 1.5px;
  }
  
  /* Make it stack neatly on small mobile screens */
  @media (max-width: 600px) {
    .profile-header {
      flex-direction: column;
      text-align: center;
      gap: 15px;
    }
  }
  
  /* Typography & Layout Styles */
  .section-heading {
    font-size: 1.7em;
    font-weight: 300;
    letter-spacing: -0.5px;
    margin-top: 0;
    margin-bottom: 20px;
    color: #24292e;
  }
  .section-divider {
    border: 0;
    border-top: 1px solid #e1e4e8; /* Light grey horizontal line */
    margin: 40px 0; /* Generous whitespace above and below the line */
  }
  .bio-intro {
    font-size: 1.1em;
    line-height: 1.6;
  }
  .custom-bio-list {
    margin-bottom: 0;
  }
  .custom-bio-list li {
    margin-bottom: 18px;
    line-height: 1.7;
    font-size: 1.05em;
  }

  /* Portfolio Grid Styles */
  .portfolio-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 20px;
    margin-top: 30px;
    margin-bottom: 40px;
  }
  .project-card {
    border: 1px solid #e1e4e8;
    border-radius: 8px;
    padding: 20px;
    background-color: #ffffff;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  .category-banner {
    background-color: #24292e;
    color: #ffffff;
    text-align: center;
    font-weight: bold;
    font-size: 0.8em;
    letter-spacing: 1.5px;
    padding: 6px;
    margin: -20px -20px 20px -20px; 
    border-radius: 7px 7px 0 0;
  }
  .project-title {
    margin-top: 0;
    margin-bottom: 10px;
    font-size: 1.25em;
  }
  .btn-primary, .btn-secondary {
    display: inline-block;
    padding: 8px 14px;
    border-radius: 6px;
    text-decoration: none;
    font-size: 0.9em;
    margin-top: 10px;
    margin-right: 10px;
  }
  .btn-primary {
    background-color: #24292e;
    color: #ffffff !important;
  }
  .btn-primary:hover {
    background-color: #000000;
  }
  .btn-secondary {
    background-color: #fafbfc;
    color: #24292e !important;
    border: 1px solid #d1d5da;
  }
  .btn-secondary:hover {
    background-color: #f3f4f6;
  }
</style>

<div class="profile-header">
  <img src="files/profile-pic.jpg" alt="Ignacio Jiménez" class="profile-pic">
  <div class="profile-info">
    <h1 class="profile-name">Ignacio Jiménez</h1>
    <p class="profile-role">Junior Data Analyst</p>
  </div>
</div>

<h2 class="section-heading">About me</h2>
<div class="bio-intro">
  Hi there! I'm a Junior Data Analyst with a unique background blending Anthropology and an MS in Computational Social Science. I am passionate about mapping the messy, organic complexity of nature and human societies into elegant, functional data models.
</div>

<hr class="section-divider">

<h2 class="section-heading">What I do</h2>
<ul class="custom-bio-list">
  <li><strong>Data Automation:</strong> Building automated data pipelines through clean, robust, and legible code, allowing my meticulous nature to really shine.</li>
  <li><strong>Visualization and Storytelling:</strong> Crafting beautiful and compelling visual narratives that turn raw data into actionable insights.</li>
  <li><strong>Causal Inference and Research Design:</strong> Applying strict scientific rigor to real problems, through research anchored in solid methodology, allowing me to measure the true effects of strategic initiatives, policies, and business decisions.</li>
  <li><strong>Clear comunication:</strong> A decade of independent science tutoring has honed my ability to explain complex, technical concepts to non-technical audiences.</li>
</ul>

<hr class="section-divider">

<h2 class="section-heading">Featured Projects</h2>

<div class="portfolio-grid">

  <!-- Project 1: Dark Souls -->
  <div class="project-card">
    <div class="category-banner">SCRAPING</div>
    <h3 class="project-title">Wiki scraper for Dark Souls videogames</h3>
    <p>An automated web scraper that extracts and structures item descriptions from the Dark Souls videogames wiki into a clean dataset for text analysis.</p>
    <a href="https://github.com/IggyWake/dark-souls-webscraper/blob/main/README.md" class="btn-primary" target="_blank">Project Readme</a>
    <a href="https://github.com/IggyWake/dark-souls-webscraper/tree/main/data" class="btn-secondary" target="_blank">Files</a>
    <a href="https://github.com/IggyWake/dark-souls-webscraper/tree/main/code" class="btn-secondary" target="_blank">Code</a>
    
  </div>

  <!-- Project 2: NetLogo -->
  <div class="project-card">
    <div class="category-banner">SIMULATION</div>
    <h3 class="project-title">Evolutionary Spread of Altruist Traits</h3>
    <p>An agent-based simulation mapping the evolutionary dynamics of cooperative traits across overlapping groups.</p>
    <a href="https://github.com/IggyWake/ebb-and-flow/blob/main/README.md" class="btn-primary" target="_blank">Project Readme</a>
    <a href="https://github.com/IggyWake/iggywake.github.io/blob/master/files/the_ebb_and_flow_of_cooperation.pdf" class="btn-secondary" target="_blank">View Paper</a>
    <a href="https://github.com/IggyWake/ebb-and-flow/tree/main/code" class="btn-secondary" target="_blank">Code</a>
  </div>

  <!-- Project 3: Cibervoluntarios -->
  <div class="project-card">
    <div class="category-banner">ANALYSIS</div>
    <h3 class="project-title">Fundación Cibervoluntarios Data Report</h3>
    <p>An independent, comprehensive data report analyzing operational metrics and community impact, translating real-world systems into actionable insights.</p>
    <a href="PATH_TO_YOUR_REPORT.pdf" class="btn-primary" download>View Report (PDF)</a>
    <a href="https://github.com/IggyWake/iggywake.github.io/tree/master/code" class="btn-secondary" target="_blank">Code</a>
  </div>

  <!-- Project 4: Baseball Statcast -->
  <div class="project-card">
    <div class="category-banner">VISUALIZATION</div>
    <h3 class="project-title">Baseball Statcast Data Visualizations</h3>
    <p>A data visualization exercise consisting on replicating and reimagining a graph representing all 113.145 balls thrown in play in all US baseball leagues in 2016.</p>
    <a href="https://github.com/IggyWake/iggywake.github.io/blob/master/files/process_report.html" class="btn-primary" target="_blank">Process Report</a>
    <a href="https://github.com/IggyWake/statcast-baseball-dataviz/tree/main/graphs" class="btn-secondary" target="_blank">Graphs</a>
    <a href="https://github.com/IggyWake/statcast-baseball-dataviz/tree/main/code" class="btn-secondary" target="_blank">Code</a>
  </div>

</div>
