import SHORT_VIDEO from '../entities/ExampleVideo.js'

export default class VideoService {
  static CHUNK_SIZE = 1024 * 1024
  static VIDEO = SHORT_VIDEO

  currentTime

  constructor() {
    this.currentTime = 0
    this.#incrementTime()
  }

  #incrementTime() {
    const interval = setInterval(() => {
      console.log(`Current Time: ${this.currentTime}`)
      this.currentTime += 1

      if (this.currentTime >= VideoService.VIDEO.duration) {
        console.log('end of video')
        clearInterval(interval)
        this.currentTime = -1
      }
    }, 1000)
  }

  calculateVideoInfo(range) {
    console.log({ range })

    const start = Number(range.match(/\d+/)[0])
    const end = Math.min(start + VideoService.CHUNK_SIZE, VideoService.VIDEO.size - 1)
    const contentLength = end - start + 1

    return { start, end, contentLength }
  }
}
