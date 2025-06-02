#!/usr/bin/env bash

IMAGE_REPO_default="cpal"
IMAGE_REPO="${IMAGE_REPO:-${IMAGE_REPO_default}}"
IMAGE_PATH_default="dice-website"
IMAGE_PATH=${IMAGE_PATH:-${IMAGE_PATH_default}}

usage() {
	echo "Render this quarto project using the tumblR created container image."
	echo "The project directory will be used as the input to rendering, and"
	echo "the ouput will be within the docs/ directory (as specified by the"
	echo "_quarto.yml configuration)."
	echo
	echo "Usage: $(basename $0) <image-tag>"
	echo
	echo "Environment (optional):"
	echo "  IMAGE_REPO: [${IMAGE_REPO_default}] alternate container image repository"
	echo "  IMAGE_PATH: [${IMAGE_PATH_default}] alternate path to image containing R"
	echo
	echo "Arguments:"
	echo "  image-tag: The tag for image path ${IMAGE_REPO}/${IMAGE_PATH}:<tag>"
}

if [ -z "$1" ]; then
	usage
	exit 1
fi
IMAGE_TAG_default="$1"

docker run --rm --mount type=bind,src=./,dst=/app/src "${IMAGE_REPO}/${IMAGE_PATH}:${IMAGE_TAG}"
