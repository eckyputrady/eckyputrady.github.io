# StaticGen

Static site generator in Haskell. This doesn't try to be smart. The configuration is the code.

## Development Mode

Start the `ghcid` daemon so that your change is published immediately.

```bash
ghcid -c "stack ghci --no-load" --test "Lib.someFunc" --reload "input" --restart "package.yaml"
```

Then setup an HTTP server in `output` folder. For example, by using python:

```bash
cd output
python -m SimpleHTTPServer
```

## Deploying

There's no need for build step since entering development mode already build a deployment-ready artifact.

Now, we deploy by pushing to master branch.

```bash
mv output /tmp/output
git checkout master
mv .git /tmp/.git
rm -rf *
mv /tmp/output/* .
rm -rf /tmp/output
mv /tmp/.git .git
git add -A
git commit -am "New version"
git push origin master
```
