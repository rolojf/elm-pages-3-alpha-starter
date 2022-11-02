import { defineConfig } from "vite";

import adapter from "./adapter.mjs";
import { ViteWebfontDownload } from "vite-plugin-webfont-dl";

export default {
  vite: defineConfig({
    plugins: [
      ViteWebfontDownload([
             "https://fonts.googleapis.com/css2?family=Fira+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap&subset=latin,latin-ext",
             "https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@100;200;300;400;500;600;700;800;900&display=swap&subset=latin,latin-ext"
      ]),
    ],
}),
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
