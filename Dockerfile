FROM haskell:9.4.8

WORKDIR /opt/hlc

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./HomeLoanCalculator.cabal /opt/hlc/HomeLoanCalculator.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build server --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/hlc
RUN cabal build server
RUN cabal install server

EXPOSE 3010
CMD ["server"]