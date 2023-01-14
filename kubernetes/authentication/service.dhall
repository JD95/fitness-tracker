let kubernetes = (../prelude.dhall).kubernetes

let NatOrString = kubernetes.NatOrString

let service =
      kubernetes.Service::{
      , metadata = kubernetes.ObjectMeta::{
        , name = Some "authentication"
        , labels = Some (toMap { app = "authentication" })
        }
      , spec = Some kubernetes.ServiceSpec::{
        , type = Some "NodePort"
        , selector = Some (toMap { app = "authentication" })
        , ports = Some
          [ kubernetes.ServicePort::{
            , protocol = Some "TCP"
            , name = Some "authentication"
            , port = 8080
            , targetPort = Some (NatOrString.Nat 8080)
            }
          ]
        }
      }

in  service
