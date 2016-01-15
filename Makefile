make:
	elm-make ElmInSpace.elm --output ElmInSpace.js
deploy: make
	rsync -rlvt ElmInSpace.{html,js} img edis:/var/www/
