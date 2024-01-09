/* @flow */

if (navigator.storage) {
  const persist: Promise<boolean> = navigator.storage.persist(); // correct
  const persisted: Promise<boolean> = navigator.storage.persisted(); // correct

  if (navigator.storage.estimate) {
    navigator.storage.estimate().then((estimate: StorageEstimate) => {
      const usage: number = estimate.usage; // correct
      const quota: number = estimate.quota; // correct
      const usageDetails: { [StorageManagerRegisteredEndpoint]: number } = // incorrect
        estimate.usageDetails;
    });
  }
}
