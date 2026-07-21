import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { nodeResolve } from "@rollup/plugin-node-resolve";

function findRepositoryRoot(startDirectory) {
  let directory = startDirectory;

  while (true) {
    if (existsSync(resolve(directory, ".git"))) {
      return directory;
    }

    const parent = dirname(directory);
    if (parent === directory) {
      return startDirectory;
    }

    directory = parent;
  }
}

function melangeEntry() {
  let entry = "./generated/src/app.mjs";

  return {
    name: "melange-entry",
    configResolved(config) {
      if (existsSync(resolve(config.root, "output/src/app.mjs"))) {
        entry = "./output/src/app.mjs";
      }
    },
    transformIndexHtml: {
      order: "pre",
      handler(html) {
        return html.replace("./generated/src/app.mjs", entry);
      },
    },
  };
}

export default {
  envDir: findRepositoryRoot(process.cwd()),
  build: {
    outDir: "./dist",
  },
  plugins: [melangeEntry(), nodeResolve()],
};
