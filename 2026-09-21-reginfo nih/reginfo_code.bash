##Shell code to automagically pull data from reginfo used in analysis using 
# CS Marcum's reginfo-tools python toolkit
# Data pulled for Good Science Blog Post
# Last Updated: 9/20/2026

git clone https://github.com/cmarcum/reginfo-tools.git
cd reginfo-tools
pip install -r requirements.txt
python eo-reg-search.py agencyCode=0900 subAgencyCode=0925 eoStatusCode=CD conclusionStartDate=01/01/1990 conclusionEndDate=09/20/2026 --output nih_regs_concluded.csv --delay 2
python eo-reg-search.py agencyCode=0900 subAgencyCode=0925 eoStatusCode=PR conclusionStartDate=01/01/1990 conclusionEndDate=09/20/2026 --output nih_regs_concluded.csv --delay 2
python eo-reg-search.py agencyCode=3145 eoStatusCode=CD conclusionStartDate=01/01/1990 conclusionEndDate=09/20/2026 --output nsf_regs_concluded.csv --delay 2
