#
rsync -arv --exclude=local_data --exclude=figure --exclude=byroo --exclude=run.R --exclude='2015_jani.Rproj' --exclude=.Rhistory --exclude=.git --exclude=.gitignore --exclude=.Rproj.user ~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/_site/ muuankarski@kapsi.fi:sites/muuankarski.kapsi.fi/www/aisurvey/