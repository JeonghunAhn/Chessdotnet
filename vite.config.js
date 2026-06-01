import { defineConfig } from "vite";

export default defineConfig({
  base: "/Chessdotnet/",
  server: {
    watch: {
      ignored: [
        "**/bin/**",
        "**/obj/**",
        "**/dist/**",
        "**/tools/PstTuner/data/**"
      ]
    }
  }
});
