# hooks: simple stupid ci/cd
1. push to main
2. webhook notification
3. validate push event
5. fetch from main
6. build
7. deploy
8. notify
9. repeat
10. profit

## github webhook setup
- payload url: `https://your-hooks-server.com/events`
- content type: `application/json`
- secret: match your `HOOKER` environment variable

## environment variables
- `HOOKER`: webhook secret
- `PORT`: optional, defaults to 8888

## mkConfig
- hooks root dir (relative to $HOME)
- notify url

## mkRepo
- repository dir (relative to hooks root dir)
- repository remote clone url
- repository files/directories to be deployed with executable

## $HOME/your-hooks-root-dir/hooked-bin/process-env.yaml
```yaml
- process: executable-name
  environment:
    - ['KEY1', 'value1']
    - ['KEY2', 'value2']

- process: another-executable
  environment:
    - ['KEY3', 'value3']
    - ['KEY4', 'value4']
```
## successful deployment notification
- post request with custom header: `hooker-signature-256` (HOOKER signed body)
- json payload containing repository url and version
