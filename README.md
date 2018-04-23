# Bitcoin-dashboard
Bitcoin-dashboard is a Shiny/R project able to visualize charts about Bitcoin price and performances.

Bitcoin-dashboard is able to show:

- the value of Bitcoin in EUR and USD in the last 24 hours and in last 30 days
- the mining of Bitcoin in the last 24 hours and in last 10 days
- the performances of Bitcoin in terms of blocks free space and delay in the last 24 hours and in last 10 days
- the performance of mempool in last hours and the prices of transactions still present in mempool
- the transactions connected with one bitcoin wallet address.

# Make it work
Execute the script get_data.R every hour in order to fetch new datasets, the enjoy the charts available with this simple shiny app.

All the code is available at https://github.com/valerio-vaccaro/Bitcoin-dashboard

The dataset is generate using free API from btc.com, blockchair.com and bitcoinaverage.com.

# License
Copyright (c) 2017-2018 Valerio Vaccaro http://www.valeriovaccaro.it

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
