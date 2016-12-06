function FindProxyForURL(url, host) {
    var str = host.toLowerCase()
    switch (true) {
    case /keg.ertos.in.nicta.com.au/.test(str): return "SOCKS5 localhost:9090; DIRECT";
    case /bamboo/.test(str): return "SOCKS5 localhost:9090; DIRECT";
    default: return "DIRECT";
    }
}
