module ZI_NS = struct
  let ns = "http://zero-install.sourceforge.net/2004/injector/interface";;
end;;

module ZI = Qdom.NsQuery (ZI_NS);;
