FROM i386/debian:jessie-slim

COPY ABC/abc /usr/local/bin/
COPY ABC/abckeys /usr/local/bin/
COPY ABC/abc.hlp /usr/local/lib/abc/
COPY ABC/abc.msg /usr/local/lib/abc/

ENTRYPOINT [ "/usr/local/bin/abc" ]
