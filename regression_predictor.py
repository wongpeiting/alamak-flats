"""
Pure-Python OLS prediction using exported Model 10 coefficients.
No R or rpy2 dependency — just JSON + numpy.

Usage:
    from regression_predictor import RegressionPredictor
    predictor = RegressionPredictor('data/model_coefficients.json')
    predicted_price = predictor.predict(row)
"""

import json
import numpy as np
import pandas as pd


class RegressionPredictor:
    def __init__(self, coefficients_path):
        with open(coefficients_path) as f:
            data = json.load(f)

        self.coefs = data['coefficients']
        self.baselines = data['baselines']
        self.factor_levels = data['factor_levels']
        self.metadata = data['metadata']
        self.intercept = self.coefs['(Intercept)']

        # Pre-index categorical coefficients for fast lookup
        self._cat_coefs = {}
        for var_name in ['town', 'flat_type', 'flat_model_grouped', 'month_factor']:
            self._cat_coefs[var_name] = {}
            for level in self.factor_levels[var_name]:
                # R names dummies as e.g. "townBEDOK", "flat_type3 ROOM"
                key = f"{var_name}{level}"
                if key in self.coefs:
                    self._cat_coefs[var_name][level] = self.coefs[key]
                else:
                    # Baseline level (coefficient = 0)
                    self._cat_coefs[var_name][level] = 0.0

        # Latest month factor (fallback for unseen months)
        self._latest_month = max(self.factor_levels['month_factor'])

        # Continuous variable names (must match R model formula exactly)
        self._continuous_vars = [
            'floor_area_sqm', 'storey_mid',
            'remaining_lease_years', 'remaining_lease_sq',
            'dist_cbd_km', 'mrt_dist_m', 'hawker_dist_m',
            'popular_school_dist_m',
            'park_dist_m', 'hospital_dist_m',
            'columbarium_dist_m', 'temple_dist_m', 'coast_dist_m',
            'num_eights_tail', 'price_has_168',
            'block_has_4', 'cny_month',
        ]

    def predict(self, row):
        """Predict resale_price for a single transaction row (dict or Series)."""
        if isinstance(row, pd.Series):
            row = row.to_dict()

        pred = self.intercept

        # Continuous variables
        for var in self._continuous_vars:
            val = row.get(var)
            if val is None or (isinstance(val, float) and np.isnan(val)):
                return np.nan  # Can't predict without all features
            pred += self.coefs.get(var, 0) * val

        # Categorical variables
        for var_name in ['town', 'flat_type', 'flat_model_grouped', 'month_factor']:
            level = str(row.get(var_name, ''))

            # Handle unseen month — use latest available
            if var_name == 'month_factor' and level not in self._cat_coefs[var_name]:
                level = self._latest_month

            # Handle unseen flat_model — map to "Other"
            if var_name == 'flat_model_grouped' and level not in self._cat_coefs[var_name]:
                level = 'Other'

            pred += self._cat_coefs[var_name].get(level, 0.0)

        return pred

    def predict_batch(self, df):
        """Predict for a DataFrame. Returns a Series of predicted prices."""
        return df.apply(self.predict, axis=1)


if __name__ == '__main__':
    # Quick test
    predictor = RegressionPredictor('data/model_coefficients.json')
    print(f"Model loaded: {predictor.metadata['n_obs']} obs, R²={predictor.metadata['r_squared']:.4f}")
    print(f"Training period: {predictor.metadata['date_range_start']} to {predictor.metadata['date_range_end']}")
    print(f"Coefficients: {len(predictor.coefs)}")
    print(f"Latest month factor: {predictor._latest_month}")
