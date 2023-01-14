let kubernetes = (../prelude.dhall).kubernetes

let deploymentLabel = toMap { app = "fitness-tracker-deployment" }

let serverContainer =
      kubernetes.Container::{
      , name = "fitness-tracker"
      , image = Some "fitness-tracker:latest"
      , imagePullPolicy = Some "IfNotPresent"
      , ports = Some [ kubernetes.ContainerPort::{ containerPort = 8081 } ]
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
          , spec = Some kubernetes.PodSpec::{ containers = [ serverContainer ] }
          }
        }
      }

in  deployment
