function FindProxyForURL(url, host) {
  switch (host.toLowerCase()) {
    case "bamboo.keg.ertos.in.nicta.com.au": return "SOCKS5 localhost:9090";
    case "bamboo" : return "SOCKS5 localhost:9090";
    case "bitbucket.keg.ertos.in.nicta.com.au": return "SOCKS5 localhost:9090";
    case "tracker.research.nicta.com.au" : return "SOCKS5 localhost:9090";
    case "wiki.inside.nicta.com.au" : return "SOCKS5 localhost:9090";
    default: return "DIRECT";
  }
}
