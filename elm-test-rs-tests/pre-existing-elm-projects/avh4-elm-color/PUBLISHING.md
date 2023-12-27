
# How to publish this package

- `npm test`
- `npx elm bump`
- Commit any changes
- `npx elm publish`, which should fail saying there is no tag
- Use the provided commands from the error message to create and push the tag
- `npx elm publish`, which should now succeed
