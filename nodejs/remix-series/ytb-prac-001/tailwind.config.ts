import type { Config } from 'tailwindcss'
import daisyui from 'daisyui'

export default {
  content: ['./app/**/{**,.client,.server}/**/*.{js,jsx,ts,tsx}'],
  safelist: [
    'flex',
    {
      pattern: /^(bg-|text-|gap-|w-|h-|justify-|items-)/,
    },
  ],
  theme: {
    extend: {
      fontFamily: {
        sans: [
          'Inter',
          'ui-sans-serif',
          'system-ui',
          'sans-serif',
          'Apple Color Emoji',
          'Segoe UI Emoji',
          'Segoe UI Symbol',
          'Noto Color Emoji',
        ],
      },
    },
  },
  plugins: [daisyui],
  daisyui: {
    themes: ['', 'winter', 'cupcake', 'luxury', 'cyberpunk'],
  },
  darkMode: ['selector', '[data-theme="luxury"]'],
} satisfies Config
