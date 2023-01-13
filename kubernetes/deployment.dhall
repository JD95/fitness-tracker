let kubernetes =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/package.dhall
        sha256:705f7bd1c157c5544143ab5917bdc3972fe941300ce4189a8ea89e6ddd9c1875

let deployment =
      kubernetes.Deployment::{
      , metadata = kubernetes.ObjectMeta::{
        , name = Some "fitness-tracker-deployment"
        }
      , spec = Some kubernetes.DeploymentSpec::{
        , selector = kubernetes.LabelSelector::{
          , matchLabels = Some (toMap { name = "fitness-tracker" })
          }
        , replicas = Some 1
        , template = kubernetes.PodTemplateSpec::{
          , metadata = Some kubernetes.ObjectMeta::{
            , name = Some "fitness-tracker"
            }
          , spec = Some kubernetes.PodSpec::{
            , containers =
              [ kubernetes.Container::{
                , name = "fitness-tracker"
                , image = Some "fitness-tracker:latest"
                , ports = Some
                  [ kubernetes.ContainerPort::{ containerPort = 8081 } ]
                }
              ]
            }
          }
        }
      }

in  deployment
