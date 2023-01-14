let kubernetes = (../prelude.dhall).kubernetes

let NatOrString = kubernetes.NatOrString

let service =
      kubernetes.Service::{
      , metadata = kubernetes.ObjectMeta::{ name = Some "fitness-tracker" }
      , spec = Some kubernetes.ServiceSpec::{
        , type = Some "NodePort"
        , selector = Some (toMap { app = "fitness-tracker" })
        , ports = Some
          [ kubernetes.ServicePort::{
            , protocol = Some "TCP"
            , name = Some "fitness-tracker"
            , port = 8081
            , targetPort = Some (NatOrString.Nat 8081)
            }
          ]
        }
      }

in  service
