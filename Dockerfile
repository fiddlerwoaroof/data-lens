FROM fiddlerwoaroof/sbcl-workspace:arm64-latest
COPY . /root/quicklisp/local-projects/data-lens
RUN sbcl --eval '(ql:quickload :data-lens/transducers/test)' --quit
