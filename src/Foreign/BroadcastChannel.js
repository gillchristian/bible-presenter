exports.createChannel = (name) => () => {
  const bc = new BroadcastChannel(name)

  const setOnMessage = (onMessage) => () => {
    bc.onmessage = (event) => {
      onMessage(event.data)()
    }
  }
  const postMessage = (msg) => () => { bc.postMessage(msg) }
  const close = () => bc.close()

  return { postMessage, close, setOnMessage }
}
