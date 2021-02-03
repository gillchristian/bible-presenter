module.exports = {
  purge: false,
  darkMode: false, // or 'media' or 'class'
  variants: {
    extend: {
      backgroundColor: ["checked", "disabled"],
      borderColor: ["checked"],
      borderWidth: ["hover"],
      cursor: ["disabled"],
      opacity: ["disabled"],
      ringColor: ["hover"],
      ringOffsetColor: ["hover"],
      ringOffsetWidth: ["hover"],
      ringWidth: ["hover"],
    },
  },
  plugins: [
    require("@tailwindcss/forms"),
    require("@tailwindcss/line-clamp"),
    require("tailwind-scrollbar"),
  ],
};
