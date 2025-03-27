import * as fs from 'node:fs'
import * as path from 'node:path'
import { config } from 'dotenv'

config({ path: path.join(process.cwd(), '_.env.local') })
// TODO config here
const gitRepo = `sunday00/iq-golf-temporary`
const token = process.env.DEV_GIT_TOKEN

async function download(info: {
  url: string
  type: string
  name: string
  download_url: string
}): Promise<void> {
  if (info.name === '_template') return

  const folder = path.join(process.cwd(), 'app', 'entities')
  const files = await fetch(
    // TODO config here
    `https://api.github.com/repos/${gitRepo}/contents/src/domains/${info.name}/struct`,
    {
      method: 'GET',
      headers: {
        Accept: 'application/vnd.github+json',
        Authorization: `Bearer ${token}`,
        'X-GitHub-Api-Version': '2022-11-28',
      },
    },
  ).then((res) => res.json())

  if (!fs.existsSync(folder)) fs.mkdirSync(folder)

  for (const file of files) {
    if (!file.name.includes('.entity.ts')) continue

    // TODO config here
    const rawUrl = `https://raw.githubusercontent.com/${gitRepo}/dev/src/domains/${info.name}/struct/${file.name}`
    let content = await fetch(rawUrl, {
      method: 'GET',
      headers: {
        Accept: 'text/html',
        Authorization: `Bearer ${token}`,
        'X-GitHub-Api-Version': '2022-11-28',
      },
    }).then((res) => res.text())

    content = content.replace(/import [.|{}\s\n\r\S]+ from .+/gim, '')
    content = content.replace(/@Entity\(.+\)/gim, '')
    content = content.replaceAll(new RegExp(`export class (.+) {`, 'gim'), 'export type $1 = {')
    content = content.replaceAll(/@Primary.+/gim, '')
    content = content.replaceAll(/@Column.+/gim, '')
    content = content.replaceAll(/@Before.+/gim, '')
    content = content.replaceAll(/private before(Insert|Update)[\n\s\S]*\n\s\s}/gim, '')
    content = content.replaceAll(/@CreateDateColumn.+/gim, '')
    content = content.replaceAll(/@UpdateDateColumn.+/gim, '')
    content = content.replaceAll(': Timestamp', ': string')
    content = content.replaceAll(/\n(\s+)\n/gm, '\n')
    content = content.replaceAll(/}\nexport/gm, '}\n\nexport')

    content = content.trim() + '\n'

    const filePath = path.join(folder, file.name)

    fs.writeFileSync(filePath, content, 'utf8')
  }
}

async function main() {
  // TODO config here
  const root_dir_url = `https://api.github.com/repos/${gitRepo}/contents/src/domains`
  const root_result = await fetch(root_dir_url, {
    method: 'GET',
    headers: {
      Accept: 'application/vnd.github+json',
      Authorization: `Bearer ${token}`,
      'X-GitHub-Api-Version': '2022-11-28',
    },
  }).then((res) => res.json())

  await Promise.all(root_result.map(download))

  return { success: true, message: '' }
}

main().then((r) => {
  if (r.success) console.log('success')
  else {
    console.error(r.message)
  }
})
