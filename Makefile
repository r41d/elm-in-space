make:
	elm-make ElmInSpace.elm --output ElmInSpace.js
deploy: make
	rsync -rltv ElmInSpace.{html,js} img edis:/var/www/
format:
	elm-format ElmInSpace.elm --output Formatted.elm
