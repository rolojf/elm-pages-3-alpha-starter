import { defineConfig } from "vite";

import adapter from "./adapter.mjs";

export default {
  vite: defineConfig({}),
  adapter,
  headTagsTemplate: (context) => `
<link rel="stylesheet" href="/style.css" />
<meta charset="UTF-8" />
<meta name="viewport" content="width=device-width,initial-scale=1" />
<meta name="generator" content="elm-pages v${context.cliVersion}" />
<meta name="mobile-web-app-capable" content="yes" />
<meta name="theme-color" content="#fffff" />
<meta name="apple-mobile-web-app-capable" content="yes" />
<meta
  name="apple-mobile-web-app-status-bar-style"
  content="black-translucent"
/>
`,
};
