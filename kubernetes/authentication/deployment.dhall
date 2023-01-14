let kubernetes = (../prelude.dhall).kubernetes

let deploymentLabel = toMap { app = "authentication" }

let keycloakContainer =
      kubernetes.Container::{
      , name = "keycloak"
      , image = Some "quay.io/keycloak/keycloak:20.0.3"
      , ports = Some [ kubernetes.ContainerPort::{ containerPort = 8080 } ]
      , env = Some
        [ kubernetes.EnvVar::{ name = "KEYCLOAK_ADMIN", value = Some "admin" }
        , kubernetes.EnvVar::{
          , name = "KEYCLOAK_ADMIN_PASSWORD"
          , value = Some "admin"
          }
        ]
      , command = Some [ "start-dev" ]
      }

let deployment =
      kubernetes.Deployment::{
      , metadata = kubernetes.ObjectMeta::{
        , name = Some "authentication-deployment"
        }
      , spec = Some kubernetes.DeploymentSpec::{
        , selector = kubernetes.LabelSelector::{
          , matchLabels = Some deploymentLabel
          }
        , replicas = Some 1
        , template = kubernetes.PodTemplateSpec::{
          , metadata = Some kubernetes.ObjectMeta::{
            , name = Some "authentication-deployment"
            , labels = Some deploymentLabel
            }
          , spec = Some kubernetes.PodSpec::{
            , containers = [ keycloakContainer ]
            }
          }
        }
      }

in  deployment
