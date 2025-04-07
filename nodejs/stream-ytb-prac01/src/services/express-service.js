import express from 'express'
import path from 'path'
import fs from 'node:fs'
import VideoService from './video-service.js'

export default class ExpressService extends VideoService {
  static PORT = process.env.PORT || 3000
  static NODE_ENV = process.env.NODE_ENV || 'development'
  #app

  constructor() {
    super()
    this.#app = express()
  }

  #initialize() {
    this.#app.use(express.static(path.resolve('public')))
  }

  #addLoggingMiddleware() {
    this.#app.use((req, res, next) => {
      console.log(`${req.method} ${req.path} ${req.headers.range}`)
      return next()
    })
  }

  #addVideoRoute() {
    this.#app.get(`/videos`, async (req, res) => {
      const range = req.headers.range
      const { start, end, contentLength } = this.calculateVideoInfo(range)

      const headers = {
        'Content-Type': 'video/mp4',
        'Content-Length': contentLength,
        'Content-Range': `bytes ${start}-${end}/${VideoService.VIDEO.size}`,
        'Accept-Ranges': 'bytes',
      }

      res.writeHead(206, headers)
      const videoStream = fs.createReadStream(VideoService.VIDEO.path, { start, end })
      videoStream.pipe(res)
    })
  }

  #addVideoTimeRoute() {
    this.#app.get('/current-time', async (req, res) => {
      return res.send(this.currentTime.toString())
    })
  }

  start() {
    this.#initialize()
    this.#addLoggingMiddleware()
    this.#addVideoRoute()
    this.#addVideoTimeRoute()

    this.#app.listen(ExpressService.PORT, () => {
      console.log(`Express server listening on port ${ExpressService.PORT}`)
    })
  }
}
