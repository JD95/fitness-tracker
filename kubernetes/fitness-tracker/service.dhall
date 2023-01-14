let kubernetes =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/package.dhall
        sha256:705f7bd1c157c5544143ab5917bdc3972fe941300ce4189a8ea89e6ddd9c1875

let NatOrString = kubernetes.NatOrString

let service =
      kubernetes.Service::{
      , metadata = kubernetes.ObjectMeta::{
        , name = Some "fitness-tracker-service"
        }
      , spec = Some kubernetes.ServiceSpec::{
        , type = Some "NodePort"
        , ports = Some
          [ kubernetes.ServicePort::{
            , protocol = Some "TCP"
            , name = Some "web-server"
            , port = 8081
            , targetPort = Some (NatOrString.Nat 8081)
            }
          ]
        }
      }

in  service
