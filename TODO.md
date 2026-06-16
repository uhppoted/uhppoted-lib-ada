# TODO

- [x] lint
- [x] release build
- [x] format
- [ ] package & publish
```
# 1. Set your token variable
export ALR_TEMP_TOKEN="ghp_YourClassicTokenHere"

# 2. Run your Alire publish command
GH_TOKEN=$ALR_TEMP_TOKEN alr publish

# 3. Immediately delete/revoke the token via GitHub API
curl -L \
  -X DELETE \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer $ALR_TEMP_TOKEN" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/applications/grants

# 4. Clear your shell history of the token secret
unset ALR_TEMP_TOKEN
```