#
rsync -arv --exclude=byroo --exclude=run.R --exclude='2015_jani.Rproj' --exclude=.Rhistory --exclude=.git --exclude=.gitignore --exclude=.Rproj.user ~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/_site/ muuankarski@kapsi.fi:sites/muuankarski.kapsi.fi/www/aisurvey/

# rsync -arv muuankarski@kapsi.fi:sites/muuankarski.kapsi.fi/www/aisurvey/data/ ~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/