# 🚨 The alamak flat alert system 🚨
## A live radar for Singapore's most statistically surprising HDB resale transactions

Some Housing Board (HDB) flats in Singapore sell high. Some sell really high. And then there are the ones that make property analysts inhale sharply through their teeth. This auto-updating project is built to spotlight the outlier flats from the past rolling month the moment they appear.

The Alamak Flats tracker scans every new resale transaction and flags the units that blow past neighbourhood norms, historical trends, and sometimes common sense. Think of it as a seismograph for the unexpected in the city-state’s housing market.

---

### What counts as a "Alamak flat"
A flat gets flagged when it deviates sharply from what you’d expect — even after adjusting for town, block, age, floor area, and month of sale. The system evaluates each flat on four editorial dimensions:
- **Price shock:** Sold unusually high for similar flats in the same town + flat type
- **Outlier jump:** A sudden spike compared to past sales in the same block
- **Market defier:** A high-priced sale during months when the rest of the market is cooling
- **Unexplainable spike:** The model controls for size, age and town… and still goes “???”
If it makes the model squint, it makes the list.

### The Alamak Meter
Every flat gets an Alamak Score (0–100) based on how extreme its deviations are in context. Only flats scoring 80 or above make the dashboard — with a regression model explaining 90% of price variation, anything below 80 is just mild noise.
- 80–82: Eh, something off leh
- 82–84: Wah, quite jialat
- 84+: ALAMAK!

### Why build an alert system
Because Singapore’s housing market is full of micro-stories hiding inside transaction tables – stories about desirability, scarcity, superstition, renovation trends, and sometimes sheer human irrationality.
A single Alamak sale can signal:
- The emergence of a newly hot neighbourhood
- Spillover effects from million-dollar enclaves
- Shifting premiums for high floors or rare layouts
- Supply crunches
- One-off buyers with reasons the data cannot possibly guess
Each alert is a doorway into a bigger question.

### What this project is and isn't
This is not a ranking of overpriced homes. It’s not financial advice. And it’s definitely not a shame board. 
The Alamak Flats tracker is a curiosity engine - a way to visualise the edges of Singapore’s housing market, where norms blur and anomalies surface.

---

### If you want to geek out 🧠 
This project is built as part of Jonathan Soma’s Foundations class in the M.S. Data Journalism programme at Columbia Journalism School.

#### A quick overview of how it works:
1. Python ingests the HDB resale dataset daily from data.gov.sg.
2. Several models compute expected price ranges for each kind of flat.
3. Deviations — only the extreme ones — are given scores.
4. A blended Alamak Score is calculated.
5. The website updates automatically to show the past month’s outliers.

#### 🧪 A more detailed rundown:
This system transforms raw HDB resale transactions into a 0–100 Alamak Score using a multi-step pipeline.

1. **Data source**
- All resale transactions are pulled directly from the official Data.gov.sg API. (**URL**: https://data.gov.sg/collections/189/view, **Update Frequency**: Daily)
- Calculations use full historical data to maintain stable baselines.
- The public map shows only the most recent rolling month of flagged sales. This allows the page to act like a live dashboard of emerging anomalies.

2. **Data cleaning**
- resale_price, floor_area_sqm, and lease_commence_date are converted to numeric values.
- month values are converted to datetime objects.
- Any flat missing essential fields is excluded from analysis.

3. **Key derived features**
- Age of flat: Flat age is approximated as year_of_sale minus lease_commence_date.
- Size and age binning: Floor area is grouped into coarse bins (e.g., 0–40 sqm, 40–60 sqm, etc.); age is grouped into decade-like bins (0–10 years, 10–20 years, etc.). These bins are used to estimate “expected prices” in the absence of full regression models.
- Derived features feed into the “expected price” model and residual calculations
- Expected prices are estimated using all historical resale data, which provides stable comparisons but does not adjust for long-term inflation; future versions may incorporate inflation-adjusted baselines. Nevertheless, the current version is still methodologically sound, as this version uses a rolling 5-year window when estimating “expected prices” for each micro-market (town + flat type + size bin + age bin). Instead of comparing a flat to very old transactions, the model benchmarks it only against recent sales of similarly aged flats, which makes the comparison realistic in an inflationary market.

4. **Dimensions of “Alamak-ness”**
- Each flat is evaluated on four separate axes, reflecting the editorial concept of a Alamak sale.
- **(A) Price shock:** Measures how expensive a flat is compared to other transactions within the same town + flat type. Implemented as a z-score: z_town_flat = (price - group_mean) / group_std, but ultimately passed through extreme-tail scoring. Only positive z-scores (above-average prices) contribute to the Alamak score. Tiny groups (<5 sales) get their scores set to zero to avoid noise.
- **(B) Outlier jump:** Measures whether a flat’s price spikes relative to its own block's historical prices. Computed as another z-score within town + block + street_name + flat_type. Captures flats that suddenly sell for far above other units in the same block environment. Again, small groups are suppressed.
- **(C) Market defier:** Identifies flats that sell unusually high in months even when the overall market is cooling. First, month-on-month median price change is computed. Cooling months = months where median price decreases. Score combines how much the market cooled and how abnormal the flat’s price was relative to its peers. Extreme-tail scoring is applied at town + flat_type level.
- **(D) Unexplainable spike (v2.0 — regression-based):** Tests whether the flat’s sale defies expectations after accounting for 20+ variables using an OLS regression model (R²=0.90). The model controls for town, flat type, floor area, storey, remaining lease (with quadratic decay), flat model, distance to CBD, nearest MRT, hawker centre, oversubscribed primary school, park, hospital, columbarium, temple, coast, as well as superstition variables (lucky 8s, "168" pattern, block number digit 4, CNY month) and month fixed effects. The residual — the gap between the actual sale price and the model’s prediction — is z-scored within town + flat type groups and passed through extreme-tail scoring. This replaces the v1.0 group-median approach, which only controlled for town, flat type, size bin, and age bin.

5. **Normalisation**
- Each Alamak dimension is converted into a 0–1 score using an extreme-tail method. For every micro-market (such as flats in the same town and flat type), the system identifies the 90th percentile of the metric. Anything below this point is treated as normal and given a score of 0. Only values above the 90th percentile are considered meaningfully unusual.
- Those extreme values are then rescaled between the group’s cutoff and its maximum, so the most exceptional sales rise toward 1. This ensures that:
    - Only the top ~10% of truly odd transactions receive a score,
    - 100-point Alamak cases stay rare, and
    - No single sale overwhelms the scale.

6. **Alamak score calculation**
- Each of the four dimensions contributes to the overall score based on these weights: 
    - Price shock (30%),
    - Outlier jump (20%), 
    - market defier (15%), and 
    - unexplainable spike (35%).
- The regression is the best single detector, but it answers “is this price explainable by fundamentals?” The other three dimensions answer “is this price surprising *in context*?” — compared to neighbours, to the block’s history, and to market timing. Different questions, all worth asking.
    - Unexplainable spike (35%): Given the highest weightage as it is the most statistically rigorous dimension — a proper regression model (R²=0.90) controlling for 20+ variables. If a flat’s price can’t be explained after accounting for location, size, floor, remaining lease, MRT proximity, hawker proximity, and more, that’s the strongest signal that something genuinely unusual is going on.
    - Price shock (30%): The regression controls for town, but readers don’t think in regression terms. They think “how does this compare to other 4-rooms in my neighbourhood?” A flat could have a small regression residual (perfectly explained by its high floor and long lease) but still be the most expensive 4-room Yishun has ever seen. That’s newsworthy even if the model says it’s “fair value.”
    - Outlier jump (20%): The regression has no block-level variable. Two flats in the same block, same type, same floor can sell $200K apart because one was fully renovated with Italian marble and the other has the original 1985 kitchen. The regression sees them as identical. The block-level z-score catches the one that suddenly broke from its block’s pattern.
    - Market defier (15%): The regression has month fixed effects, but those capture the *average* market movement, not the *direction*. A flat selling high during a month when the overall market dipped is editorially interesting — it suggests demand that’s bucking the trend. The regression treats all transactions in that month the same.

7. **Alamak threshold**
- A flat is considered an “Alamak flat” if its score ≥ 80.
- With the regression-based Dimension D (R²=0.90), the model already explains most price variation. The threshold was raised from 70 to 80 to ensure only genuinely surprising transactions make the cut.

8. **Rolling month**
- Only flats from the most recent one month (latest month minus 1 month) appear on the public-facing map. This keeps the project living, reactive and news-friendly.

---

### Project roadmap

#### Phase 1: Data collection
- [x] Write a scraper to grab the exact data needed
- [x] Employ the help of ChatGPT to analyse the raw data and identify the Alamak flats on a rolling month basis.
- [x] Save the Alamak flat output in CSV

#### Phase 2: Automation
- [x] Make it a GitHub repository
- [x] Turn on GitHub Actions (and set it up)
- [x] Set up your .yml file (+ make sure notebook name matches)

#### Phase 3: Make website
- [x] Create index.html file
- [x] Add it to your repo and push it up to GitHub
- [x] Turn on GitHub Pages by clicking many buttons
- [x] Make sure your index.html works by visiting your page

#### Phase 4: Map visualisation
- [x] Create chart/map using Datawrapper
- [x] Make sure we're linking to our data, not uploading it
- [x] Use the 'responsive iframe' version of embedding

#### Phase 5: Update our website
- [x] Add the embed code to our index.html
- [x] Push it on up to GitHub Pages
- [x] Wait for GitHub actions to finish deploying our web page

---

##### Reflections

When I began this project, I was skeptical that I could pull off anything remotely close in less than a week. I had not worked with APIs much outside of coursework, and certainly never modelled outliers or deployed something that updates itself. But after following the steps in [this tutorial](https://www.youtube.com/watch?v=QNKxzkNpsko), before long, I found myself with a completed site in a single day. Soma did comment that my pitch was very basic, so I don't doubt that I picked a highly munchable project for myself too. My biggest takeaway is in understanding how each piece of the project's puzzle (Datawrapper, GitHub, VS Code, etc.) speak to each other, and in learning how to debug and work through ~~life's~~a data-heavy project's biggest issues with generous, generous help from ChatGPT.

##### Reflections (v2.0)

I have since taken regression classes, which have taught me how to apply multivariate regression on my prediction model. I trained my model (R²=0.90) based on 50,000+ transactions, controlling for 20+ variables including geographic distances, lease decay, and superstition factors. This replaces the group-median approach from v1.0 and was developed as part of the [HDB Regression project](https://github.com/wongpeiting/hdb-regression) using Dhrumil Mehta’s EDA-with-regression pipeline from the Columbia M.S. Data Journalism programme.

##### What’s still on the wish list

An interactive dashboard with filters, hover tooltips, and historical replay. The full 1990–2026 dataset (975,000 transactions) is now available, so the data is ready — the product just hasn’t been built yet. It would be something to see how the Alamak flats moved across the map over the past 35 years to land at where we are today.

---

### Version history

**v1.0 (Dec 2025):** Built as part of Jonathan Soma’s Foundations class in the M.S. Data Journalism programme at Columbia Journalism School. Used group medians and z-scores for anomaly detection across four editorial dimensions.

**v2.0 (Apr 2026):** Upgraded Dimension D ("unexplainable spike") with a proper OLS regression model (R²=0.90, 20+ variables) controlling for location, size, floor, remaining lease (with quadratic decay), proximity to MRT/hawker centres/oversubscribed primary schools, feng shui factors (columbarium, temple, coast distance), and superstition variables (lucky 8s, "168" pattern, block number digit 4, CNY month). Geocoding switched from ArcGIS to OneMap (official Singapore government mapping service). Weights rebalanced to give highest weight (35%) to the regression-based dimension. Model coefficients refresh monthly via automated GitHub Actions workflow. Rebranded from "WTF Flat Alert System" to "Alamak Flats."

*Created on: Dec 7, 2025*

*Last updated: Apr 19, 2026*
