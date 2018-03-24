# cntlm-password

*Changing your password for cntlm can be a little tedious*.

"[Cntlm](http://cntlm.sourceforge.net/) is an NTLM / NTLM Session Response / NTLMv2 authenticating HTTP proxy." `cntlm-password` kills `cntlm`, prompts you for your username, edits your `cntlm` conf file and restarts `cntlm`. For security reasons, your password is not echoed to the terminal. Only valid lines are modified by `cntlm-password` (i.e., malformed or commented-out lines will not be modified).

`cntlm-password` assumes that -
  * you are using a POSIX operating system
  * `cntlm` and `pkill` are installed
  * your `cntlm` conf file path is `/usr/local/etc/cntlm.conf`

### Installation

Install [Stack](https://docs.haskellstack.org/en/stable/README/) -
`curl -sSL https://get.haskellstack.org/ | sh`

Clone `cntlm-password` repository -
`git clone https://github.com/Michaelt293/cntlm-password.git`

Install -
```
cd cntlm-password
stack install
```
