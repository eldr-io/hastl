/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./static/*.{html,js}",
    "./static/**/*.{html,js}",
    "./lib/Api/Templates/**/*.hs",
  ],
  theme: {
    container: {
      center: true,
      padding: {
        DEFAULT: "1rem",
        mobile: "2rem",
        tablet: "4rem",
        desktop: "5rem",
      },
    },
    extend: {},
  },
  plugins: [require("@tailwindcss/forms"), require("@tailwindcss/typography")],
};
