exports.requestFullScreen_ = (doc) => {
  console.log(doc)
  return doc.requestFullscreen()
    .then(() => true)
    .catch(() => false)
}

exports.exitFullScreen_ = (doc) =>
  doc.exitFullscreen()
  .then(() => true)
  .catch(() => false)

exports.checkFullScreen =
  (doc) => Boolean(doc.fullscreenElement)
