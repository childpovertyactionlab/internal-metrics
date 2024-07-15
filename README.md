# CPAL Internal Metrics

Welcome to the CPAL Internal Metrics repository. This repository contains the code and resources for the Internal Metrics Dashboard hosted at [CPAL ShinyApps](https://cpal.shinyapps.io/internal-dashboard/).

## Repository Structure

- `scripts/`: This folder contains R scripts used to pull data from the `tidycensus` package and create specific geographies of interest.
- `internal-dashboard/`: This folder contains the code for the Shiny app.
- `internal-dashboard/data`: This folder contains the current data output for the shiny tool along with a data dictionary used to add new variables to the tool.

## Getting Started

To get started with the repository, clone it to your local machine using the following command:

```bash
git clone https://github.com/childpovertyactionlab/internal-metrics.git
```

## Requirements

Make sure you have R installed on your machine. You will also need to install the following R packages:

## Usage

To run the Shiny app locally, navigate to the `internal-dashboard` folder and launch the app with the following command:

```R
shiny::runApp("internal-dashboard")
```

## Data Scripts

The `scripts` folder includes scripts to pull data and process it for use in the Shiny app. Make sure to set up your `tidycensus` API key before running these scripts. You can obtain a free API key from the [Census Bureau](https://api.census.gov/data/key_signup.html) and set it up in R as follows:

```R
tidycensus::census_api_key("YOUR_API_KEY")
```

## Contributing

We welcome contributions to improve the dashboard. Please fork the repository and submit pull requests for any enhancements or bug fixes.

## License

This project is licensed under the GNU General Public License. See the [LICENSE](LICENSE) file for details.

## Contact

For any questions or support, please contact [analytics@childpovertyactionlab.org] or open an issue in this repository.

---

Thank you for using the CPAL Internal Metrics Dashboard!

---
