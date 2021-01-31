module.exports = {
  purge: false,
  darkMode: false, // or 'media' or 'class'
  variants: {
    extend: {
      backgroundColor: ["checked"],
      borderColor: ["checked"],
      borderWidth: ["hover"],
      cursor: ["disabled"],
      opacity: ["disabled"],
    },
  },
  plugins: [
    require("@tailwindcss/forms"),
    require("@tailwindcss/line-clamp"),
    require("tailwind-scrollbar"),
  ],
};
