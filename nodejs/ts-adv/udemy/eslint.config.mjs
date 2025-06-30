import globals from 'globals'
import tseslint from 'typescript-eslint'

export default [
  { files: ['**/*.{js,mjs,cjs,ts}'] },
  { languageOptions: { globals: globals.browser } },
  ...tseslint.configs.recommended,
  {
    rules: {
      'no-unused-vars': 'off',
      '@typescript-eslint/no-unused-vars': [
        'warn',
        {
          argsIgnorePattern: '^_[^_].*$|^_$',
          varsIgnorePattern: '^_[^_].*$|^_$',
          caughtErrorsIgnorePattern: '^_[^_].*$|^_$',
        },
      ],
    },
  },
]
