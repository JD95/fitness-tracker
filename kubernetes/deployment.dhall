let kubernetes =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/package.dhall
        sha256:705f7bd1c157c5544143ab5917bdc3972fe941300ce4189a8ea89e6ddd9c1875

let deploymentLabel = toMap { app = "fitness-tracker-deployment" }

let serverContainer =
      kubernetes.Container::{
      , name = "fitness-tracker"
      , image = Some "fitness-tracker:latest"
      , ports = Some [ kubernetes.ContainerPort::{ containerPort = 8081 } ]
      }

let keycloakContainer =
      kubernetes.Container::{
      , name = "authentication"
      , image = Some "quay.io/keycloak/keycloak:20.0.3"
      , ports = Some [ kubernetes.ContainerPort::{ containerPort = 8080 } ]
      , env = Some
        [ kubernetes.EnvVar::{ name = "KEYCLOAK_ADMIN", value = Some "admin" }
        , kubernetes.EnvVar::{
          , name = "KEYCLOAK_ADMIN_PASSWORD"
          , value = Some "admin"
          }
        ]
      }

let deployment =
      kubernetes.Deployment::{
      , metadata = kubernetes.ObjectMeta::{
        , name = Some "fitness-tracker-deployment"
        }
      , spec = Some kubernetes.DeploymentSpec::{
        , selector = kubernetes.LabelSelector::{
          , matchLabels = Some deploymentLabel
          }
        , replicas = Some 1
        , template = kubernetes.PodTemplateSpec::{
          , metadata = Some kubernetes.ObjectMeta::{
            , name = Some "fitness-tracker-deployment"
            , labels = Some deploymentLabel
            }
          , spec = Some kubernetes.PodSpec::{
            , containers = [ serverContainer, keycloakContainer ]
            }
          }
        }
      }

in  deployment
