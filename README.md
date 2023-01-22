# R_master_project_GARCH
Forecasting VaR of cryptocurrencies and find optimal portfolio to maximize Return/VaR ratio
Implemented 5 models of GARCH family (sGARCH, eGARCH, gjrGARCH, TGARCH, and NAGARCH) to forecast volatility of a portfolio of six cryptocurrencies with equal weights by fitting data with different distribution with rugarch package. As a result, eGARCH with skewed-t distribution had best information ratio and backtesting result. Then use Monte Carlo Stimulation to vary the portfolio weights between six crypto to find the optimal portfolio with maximum mean return/VaR ratio.
