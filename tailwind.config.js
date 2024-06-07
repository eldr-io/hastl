/** @type {import('tailwindcss').Config} */
export const content = [
    // "./static/*.{html,js}",
    // "./static/**/*.{html,js}",
    "./lib/Api/Templates/**/*.hs",
];
export const theme = {
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
};
export const plugins = [require("@tailwindcss/forms"), require("@tailwindcss/typography")];
