import type { Config } from 'tailwindcss'
import daisyui from 'daisyui'
import * as themes from 'daisyui/src/theming/themes'

const pattReg01 = /^(text-|gap-|w-|h-|font-|mb-|pb-|rounded-)/
const pattReg02 = /^(bg-|justify-|items-|btn-|modal-|alert-|hover:)/
const pattReg03 = /^(truncate)/

export default {
  content: ['./app/**/{**,.client,.server}/**/*.{js,jsx,ts,tsx}'],
  safelist: [
    'flex',
    {
      pattern: new RegExp(pattReg01.source + '|' + pattReg02.source + '|' + pattReg03.source),
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
    themes: [
      '',
      'winter',
      'cupcake',
      'luxury',
      'cyberpunk',
      {
        winter: {
          ...themes['winter'],
          '.bg-darker': {
            'background-color': 'rgba(0,0,0,0.1)',
          },
          '.bg-darker:hover': {
            'background-color': 'rgba(0,0,0,0.1)',
          },
        },
      },
    ],
  },
  darkMode: ['selector', '[data-theme="luxury"]'],
} satisfies Config
