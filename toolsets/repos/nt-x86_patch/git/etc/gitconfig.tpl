[core]
	symlinks = false
	autocrlf = false
[color]
	diff = auto
	status = auto
	branch = auto
	interactive = true
[pack]
	packSizeLimit = 2g
[help]
	format = html
[http]
	sslcainfo = __HAM_HOME__/toolsets/repos/nt-x86/git/bin/curl-ca-bundle.crt
[sendemail]
	smtpserver = /bin/msmtp.exe

[diff "astextplain"]
	textconv = astextplain
[rebase]
	autosquash = true
